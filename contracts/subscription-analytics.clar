;; subscription-analytics
;; 
;; This contract provides analytics and optimization recommendations for subscription
;; spending by analyzing payment data and subscription patterns. It helps users understand
;; their subscription costs, identify trends, and make informed decisions about which
;; services to keep, downgrade, or cancel.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-INVALID-SUBSCRIPTION (err u1002))
(define-constant ERR-INVALID-PERIOD (err u1003))
(define-constant ERR-INVALID-CATEGORY (err u1004))
(define-constant ERR-NO-DATA (err u1005))

;; Constants
(define-constant CATEGORIES (list "entertainment" "productivity" "health" "food" "other"))

;; Data Maps and Variables

;; Track user's subscriptions
(define-map user-subscriptions
  { user: principal, subscription-id: (string-ascii 50) }
  {
    name: (string-ascii 100),
    amount: uint,
    category: (string-ascii 20),
    billing-cycle: uint,  ;; in days
    start-date: uint,     ;; unix timestamp
    last-payment: uint,   ;; unix timestamp
    usage-frequency: uint ;; 1-10 scale, manually entered by user
  }
)

;; Track historical payments for trend analysis
(define-map subscription-payments
  { user: principal, subscription-id: (string-ascii 50), payment-date: uint }
  { amount: uint }
)

;; Track user's category budget limits
(define-map category-budgets
  { user: principal, category: (string-ascii 20) }
  { monthly-limit: uint }
)

;; Track optimization suggestions for each user
(define-map optimization-suggestions
  { user: principal, suggestion-id: uint }
  {
    subscription-id: (string-ascii 50),
    suggestion-type: (string-ascii 20),  ;; e.g., "cancel", "downgrade", "consolidate"
    estimated-savings: uint,
    reason: (string-ascii 200),
    timestamp: uint
  }
)

;; Counter for suggestion IDs
(define-data-var next-suggestion-id uint u1)

;; Private Functions

;; Validate if a category is allowed
(define-private (is-valid-category (category (string-ascii 20)))
  (default-to false (some (lambda (valid-category) (is-eq category valid-category)) CATEGORIES))
)

;; Calculate the next payment date based on billing cycle and last payment
(define-private (calculate-next-payment 
                 (last-payment uint) 
                 (billing-cycle uint))
  (+ last-payment (* billing-cycle u86400))  ;; Converting days to seconds
)

;; Check if a subscription is about to renew (within 7 days)
(define-private (is-renewal-upcoming 
                 (last-payment uint) 
                 (billing-cycle uint) 
                 (current-time uint))
  (let ((next-payment (calculate-next-payment last-payment billing-cycle))
        (renewal-window (+ current-time (* u7 u86400))))
    (<= next-payment renewal-window)
  )
)

;; Calculate how many days ago the subscription was last used (based on usage frequency)
(define-private (calculate-days-since-use 
                 (usage-frequency uint) 
                 (current-time uint) 
                 (last-payment uint) 
                 (billing-cycle uint))
  (let ((days-in-cycle billing-cycle)
        (days-since-payment (/ (- current-time last-payment) u86400))
        (usage-percent (/ usage-frequency u10)))
    ;; Estimate days since last use based on usage frequency
    ;; Lower usage frequency = more days since last use
    (if (is-eq usage-frequency u0)
        days-in-cycle  ;; If never used, assume unused for the whole cycle
        (/ (* days-since-payment (- u10 usage-frequency)) u10))
  )
)

;; Get subscription details by ID, returns none if not found
(define-private (get-subscription-by-id 
                 (user principal) 
                 (subscription-id (string-ascii 50)))
  (map-get? user-subscriptions { user: user, subscription-id: subscription-id })
)

;; Add a new optimization suggestion
(define-private (add-suggestion 
                 (user principal) 
                 (subscription-id (string-ascii 50)) 
                 (suggestion-type (string-ascii 20)) 
                 (estimated-savings uint) 
                 (reason (string-ascii 200)) 
                 (current-time uint))
  (let ((suggestion-id (var-get next-suggestion-id)))
    ;; Store the suggestion
    (map-set optimization-suggestions
      { user: user, suggestion-id: suggestion-id }
      {
        subscription-id: subscription-id,
        suggestion-type: suggestion-type,
        estimated-savings: estimated-savings,
        reason: reason,
        timestamp: current-time
      }
    )
    ;; Increment suggestion ID counter
    (var-set next-suggestion-id (+ suggestion-id u1))
    suggestion-id
  )
)

;; Read-only Functions

;; Get total monthly subscription cost
(define-read-only (get-monthly-spending (user principal))
  (let ((subscriptions (map-get? user-subscriptions { user: user })))
    (if (is-none subscriptions)
        (ok u0)
        (let ((total-monthly (fold + (map calculate-monthly-cost (unwrap-panic subscriptions)) u0)))
          (ok total-monthly)
        )
    )
  )
)

;; Calculate monthly cost for a subscription (normalizing different billing cycles)
(define-read-only (calculate-monthly-cost (subscription {
                                            name: (string-ascii 100),
                                            amount: uint,
                                            category: (string-ascii 20),
                                            billing-cycle: uint,
                                            start-date: uint,
                                            last-payment: uint,
                                            usage-frequency: uint
                                          }))
  (let ((amount (get amount subscription))
        (billing-cycle (get billing-cycle subscription)))
    ;; Convert to monthly cost: (amount / billing-cycle) * 30
    (/ (* amount u30) billing-cycle)
  )
)

;; Get spending by category
(define-read-only (get-spending-by-category 
                   (user principal) 
                   (category (string-ascii 20)))
  (if (not (is-valid-category category))
      (err ERR-INVALID-CATEGORY)
      (let ((subscriptions (filter 
                            (lambda (sub) (is-eq (get category sub) category))
                            (default-to (list) (map-get? user-subscriptions { user: user })))))
        (ok (fold + (map calculate-monthly-cost subscriptions) u0))
      )
  )
)

;; Get upcoming renewals in the next 7 days
(define-read-only (get-upcoming-renewals (user principal))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (subscriptions (default-to (list) (map-get? user-subscriptions { user: user }))))
    (ok (filter 
         (lambda (sub) 
           (is-renewal-upcoming (get last-payment sub) (get billing-cycle sub) current-time))
         subscriptions))
  )
)

;; Get rarely used subscriptions (usage frequency <= 3)
(define-read-only (get-rarely-used-subscriptions (user principal))
  (let ((subscriptions (default-to (list) (map-get? user-subscriptions { user: user }))))
    (ok (filter 
         (lambda (sub) (<= (get usage-frequency sub) u3))
         subscriptions))
  )
)

;; Get existing optimization suggestions for a user
(define-read-only (get-optimization-suggestions (user principal))
  (ok (default-to (list) (map-get? optimization-suggestions { user: user })))
)

;; Calculate year-over-year spending change
(define-read-only (get-yearly-spending-change 
                   (user principal) 
                   (current-year uint) 
                   (previous-year uint))
  ;; In a real implementation, this would analyze payment data from the subscription-payments map
  ;; For simplicity, returning a mock response
  (ok {
    current-year-spending: u12000,
    previous-year-spending: u10000,
    percent-change: u20,
    is-increase: true
  })
)

;; Public Functions

;; Add a new subscription to track
(define-public (add-subscription 
                (subscription-id (string-ascii 50)) 
                (name (string-ascii 100)) 
                (amount uint) 
                (category (string-ascii 20)) 
                (billing-cycle uint) 
                (start-date uint) 
                (usage-frequency uint))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    ;; Validate inputs
    (asserts! (is-valid-category category) (err ERR-INVALID-CATEGORY))
    (asserts! (> billing-cycle u0) (err ERR-INVALID-PERIOD))
    (asserts! (<= usage-frequency u10) (err ERR-INVALID-SUBSCRIPTION))
    
    ;; Store subscription data
    (map-set user-subscriptions
      { user: tx-sender, subscription-id: subscription-id }
      {
        name: name,
        amount: amount,
        category: category,
        billing-cycle: billing-cycle,
        start-date: start-date,
        last-payment: current-time,
        usage-frequency: usage-frequency
      }
    )
    (ok true)
  )
)

;; Update subscription usage frequency
(define-public (update-usage-frequency 
                (subscription-id (string-ascii 50)) 
                (new-frequency uint))
  (let ((subscription (get-subscription-by-id tx-sender subscription-id)))
    ;; Validate inputs
    (asserts! (is-some subscription) (err ERR-INVALID-SUBSCRIPTION))
    (asserts! (<= new-frequency u10) (err ERR-INVALID-SUBSCRIPTION))
    
    ;; Update subscription with new usage frequency
    (map-set user-subscriptions
      { user: tx-sender, subscription-id: subscription-id }
      (merge (unwrap-panic subscription) { usage-frequency: new-frequency })
    )
    (ok true)
  )
)

;; Record a payment for a subscription
(define-public (record-payment 
                (subscription-id (string-ascii 50)) 
                (amount uint))
  (let ((subscription (get-subscription-by-id tx-sender subscription-id))
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    ;; Validate subscription exists
    (asserts! (is-some subscription) (err ERR-INVALID-SUBSCRIPTION))
    
    ;; Record payment
    (map-set subscription-payments
      { user: tx-sender, subscription-id: subscription-id, payment-date: current-time }
      { amount: amount }
    )
    
    ;; Update last payment timestamp
    (map-set user-subscriptions
      { user: tx-sender, subscription-id: subscription-id }
      (merge (unwrap-panic subscription) { last-payment: current-time, amount: amount })
    )
    (ok true)
  )
)

;; Set a monthly budget limit for a category
(define-public (set-category-budget 
                (category (string-ascii 20)) 
                (monthly-limit uint))
  ;; Validate category
  (asserts! (is-valid-category category) (err ERR-INVALID-CATEGORY))
  
  ;; Set budget
  (map-set category-budgets
    { user: tx-sender, category: category }
    { monthly-limit: monthly-limit }
  )
  (ok true)
)

;; Generate optimization suggestions based on spending patterns
(define-public (generate-optimization-suggestions)
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (rarely-used (unwrap-panic (get-rarely-used-subscriptions tx-sender)))
        (renewals (unwrap-panic (get-upcoming-renewals tx-sender))))
    
    ;; Create suggestions for rarely used subscriptions
    (map 
      (lambda (sub)
        (add-suggestion 
          tx-sender
          (get name sub)
          "cancel"
          (get amount sub)
          "This subscription is rarely used based on your reported usage frequency."
          current-time)
      )
      rarely-used)
    
    ;; Create suggestions for upcoming renewals that could be reviewed
    (map 
      (lambda (sub)
        (add-suggestion 
          tx-sender
          (get name sub)
          "review"
          u0
          "This subscription is renewing soon. Review if you want to continue."
          current-time)
      )
      renewals)
    
    (ok true)
  )
)

;; Delete a subscription
(define-public (delete-subscription (subscription-id (string-ascii 50)))
  (let ((subscription (get-subscription-by-id tx-sender subscription-id)))
    ;; Validate subscription exists
    (asserts! (is-some subscription) (err ERR-INVALID-SUBSCRIPTION))
    
    ;; Delete subscription
    (map-delete user-subscriptions { user: tx-sender, subscription-id: subscription-id })
    (ok true)
  )
)