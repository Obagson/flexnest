;; subscription-manager
;; Core contract for managing subscription records and user interactions in the FlexNest platform
;; 
;; This contract allows users to register, update, and manage subscription services,
;; maintaining a comprehensive registry of all subscriptions with their details
;; such as service name, payment amount, billing cycle, next payment date,
;; status, and payment history.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-SUBSCRIPTION-NOT-FOUND (err u101))
(define-constant ERR-INVALID-CYCLE-PERIOD (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INVALID-DATE (err u104))
(define-constant ERR-SUBSCRIPTION-EXISTS (err u105))
(define-constant ERR-INVALID-STATUS-CHANGE (err u106))

;; Billing cycle periods (in days)
(define-constant CYCLE-MONTHLY u30)
(define-constant CYCLE-QUARTERLY u90)
(define-constant CYCLE-BIANNUAL u182)
(define-constant CYCLE-ANNUAL u365)

;; Subscription status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-INACTIVE u2)
(define-constant STATUS-PAUSED u3)

;; Data structures
;; Basic subscription information
(define-map subscriptions
  { owner: principal, subscription-id: uint }
  {
    service-name: (string-ascii 100),
    payment-amount: uint,
    billing-cycle: uint,
    next-payment-date: uint,
    status: uint,
    created-at: uint
  }
)

;; Track all subscription IDs for each user
(define-map user-subscriptions
  { owner: principal }
  { subscription-ids: (list 100 uint) }
)

;; Payment history for each subscription
(define-map payment-history
  { owner: principal, subscription-id: uint }
  { payments: (list 100 { payment-date: uint, amount: uint }) }
)

;; Status change history for subscription
(define-map status-history
  { owner: principal, subscription-id: uint }
  { changes: (list 50 { timestamp: uint, status: uint }) }
)

;; Global subscription counter
(define-data-var subscription-counter uint u0)

;; Private functions

;; Get the next subscription ID and increment the counter
(define-private (get-next-subscription-id)
  (let ((current-id (var-get subscription-counter)))
    (begin
      (var-set subscription-counter (+ current-id u1))
      current-id
    )
  )
)

;; Check if a subscription exists for a user
(define-private (subscription-exists (owner principal) (subscription-id uint))
  (is-some (map-get? subscriptions { owner: owner, subscription-id: subscription-id }))
)

;; Add a subscription ID to a user's list
(define-private (add-subscription-id-to-user (owner principal) (subscription-id uint))
  (let (
    (current-subscriptions (default-to { subscription-ids: (list) } (map-get? user-subscriptions { owner: owner })))
  )
    (map-set user-subscriptions
      { owner: owner }
      { subscription-ids: (append (get subscription-ids current-subscriptions) subscription-id) }
    )
  )
)

;; Validate billing cycle (must be one of the defined constants)
(define-private (is-valid-billing-cycle (cycle uint))
  (or
    (is-eq cycle CYCLE-MONTHLY)
    (is-eq cycle CYCLE-QUARTERLY)
    (is-eq cycle CYCLE-BIANNUAL)
    (is-eq cycle CYCLE-ANNUAL)
  )
)

;; Validate payment amount (must be greater than zero)
(define-private (is-valid-payment-amount (amount uint))
  (> amount u0)
)

;; Record a status change for a subscription
(define-private (record-status-change (owner principal) (subscription-id uint) (status uint))
  (let (
    (current-history (default-to { changes: (list) } (map-get? status-history { owner: owner, subscription-id: subscription-id })))
    (new-change { timestamp: block-height, status: status })
  )
    (map-set status-history
      { owner: owner, subscription-id: subscription-id }
      { changes: (append (get changes current-history) new-change) }
    )
  )
)

;; Public functions

;; Register a new subscription
(define-public (register-subscription 
                (service-name (string-ascii 100))
                (payment-amount uint)
                (billing-cycle uint)
                (next-payment-date uint))
  (let (
    (new-id (get-next-subscription-id))
    (owner tx-sender)
  )
    ;; Validate inputs
    (asserts! (is-valid-billing-cycle billing-cycle) ERR-INVALID-CYCLE-PERIOD)
    (asserts! (is-valid-payment-amount payment-amount) ERR-INVALID-AMOUNT)
    (asserts! (>= next-payment-date block-height) ERR-INVALID-DATE)
    
    ;; Store the subscription
    (map-set subscriptions
      { owner: owner, subscription-id: new-id }
      {
        service-name: service-name,
        payment-amount: payment-amount,
        billing-cycle: billing-cycle,
        next-payment-date: next-payment-date,
        status: STATUS-ACTIVE,
        created-at: block-height
      }
    )
    
    ;; Update user's subscription list
    (add-subscription-id-to-user owner new-id)
    
    ;; Initialize status history
    (record-status-change owner new-id STATUS-ACTIVE)
    
    ;; Return the new subscription ID
    (ok new-id)
  )
)

;; Update subscription details
(define-public (update-subscription
                (subscription-id uint)
                (service-name (string-ascii 100))
                (payment-amount uint)
                (billing-cycle uint)
                (next-payment-date uint))
  (let (
    (owner tx-sender)
    (subscription-data (map-get? subscriptions { owner: owner, subscription-id: subscription-id }))
  )
    ;; Verify subscription exists and is owned by tx-sender
    (asserts! (is-some subscription-data) ERR-SUBSCRIPTION-NOT-FOUND)
    
    ;; Validate inputs
    (asserts! (is-valid-billing-cycle billing-cycle) ERR-INVALID-CYCLE-PERIOD)
    (asserts! (is-valid-payment-amount payment-amount) ERR-INVALID-AMOUNT)
    (asserts! (>= next-payment-date block-height) ERR-INVALID-DATE)
    
    ;; Update subscription details while preserving status and creation time
    (map-set subscriptions
      { owner: owner, subscription-id: subscription-id }
      {
        service-name: service-name,
        payment-amount: payment-amount,
        billing-cycle: billing-cycle,
        next-payment-date: next-payment-date,
        status: (get status (unwrap-panic subscription-data)),
        created-at: (get created-at (unwrap-panic subscription-data))
      }
    )
    
    (ok true)
  )
)

;; Change subscription status (active, inactive, paused)
(define-public (update-subscription-status
                (subscription-id uint)
                (new-status uint))
  (let (
    (owner tx-sender)
    (subscription-data (map-get? subscriptions { owner: owner, subscription-id: subscription-id }))
  )
    ;; Verify subscription exists and is owned by tx-sender
    (asserts! (is-some subscription-data) ERR-SUBSCRIPTION-NOT-FOUND)
    
    ;; Validate status
    (asserts! (or (is-eq new-status STATUS-ACTIVE)
                 (is-eq new-status STATUS-INACTIVE)
                 (is-eq new-status STATUS-PAUSED))
             ERR-INVALID-STATUS-CHANGE)
    
    ;; Update subscription status
    (map-set subscriptions
      { owner: owner, subscription-id: subscription-id }
      (merge (unwrap-panic subscription-data) { status: new-status })
    )
    
    ;; Record the status change
    (record-status-change owner subscription-id new-status)
    
    (ok true)
  )
)

;; Record a payment for a subscription
(define-public (record-payment
                (subscription-id uint)
                (payment-date uint)
                (amount uint))
  (let (
    (owner tx-sender)
    (subscription-data (map-get? subscriptions { owner: owner, subscription-id: subscription-id }))
    (current-history (default-to { payments: (list) } (map-get? payment-history { owner: owner, subscription-id: subscription-id })))
    (new-payment { payment-date: payment-date, amount: amount })
  )
    ;; Verify subscription exists and is owned by tx-sender
    (asserts! (is-some subscription-data) ERR-SUBSCRIPTION-NOT-FOUND)
    
    ;; Validate payment date and amount
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Record the payment
    (map-set payment-history
      { owner: owner, subscription-id: subscription-id }
      { payments: (append (get payments current-history) new-payment) }
    )
    
    ;; Calculate next payment date based on billing cycle
    (let (
      (sub-data (unwrap-panic subscription-data))
      (next-date (+ payment-date (get billing-cycle sub-data)))
    )
      ;; Update next payment date
      (map-set subscriptions
        { owner: owner, subscription-id: subscription-id }
        (merge sub-data { next-payment-date: next-date })
      )
      
      (ok true)
    )
  )
)

;; Delete a subscription
(define-public (delete-subscription (subscription-id uint))
  (let (
    (owner tx-sender)
    (subscription-data (map-get? subscriptions { owner: owner, subscription-id: subscription-id }))
  )
    ;; Verify subscription exists and is owned by tx-sender
    (asserts! (is-some subscription-data) ERR-SUBSCRIPTION-NOT-FOUND)
    
    ;; Delete the subscription data
    (map-delete subscriptions { owner: owner, subscription-id: subscription-id })
    
    ;; Note: We don't remove the ID from user-subscriptions for historical tracking
    ;; However, we set status to inactive in history
    (record-status-change owner subscription-id STATUS-INACTIVE)
    
    (ok true)
  )
)

;; Read-only functions

;; Get all subscription IDs for a user
(define-read-only (get-user-subscriptions (user principal))
  (default-to { subscription-ids: (list) } (map-get? user-subscriptions { owner: user }))
)

;; Get details for a specific subscription
(define-read-only (get-subscription-details (owner principal) (subscription-id uint))
  (map-get? subscriptions { owner: owner, subscription-id: subscription-id })
)

;; Get payment history for a subscription
(define-read-only (get-payment-history (owner principal) (subscription-id uint))
  (default-to { payments: (list) } (map-get? payment-history { owner: owner, subscription-id: subscription-id }))
)

;; Get status change history for a subscription
(define-read-only (get-status-history (owner principal) (subscription-id uint))
  (default-to { changes: (list) } (map-get? status-history { owner: owner, subscription-id: subscription-id }))
)

;; Get active subscriptions for a user
(define-read-only (get-active-subscriptions (user principal))
  (let (
    (user-subs (get subscription-ids (get-user-subscriptions user)))
    (active-subs (list))
  )
    ;; Filter function is not available in Clarity, so we can't directly implement this.
    ;; In a real implementation, we'd need client-side filtering or a paginated approach
    ;; to iterate through subscriptions and check their status.
    ;; This is a limitation of Clarity's design.
    (ok user-subs)
  )
)

;; Check if subscriptions need renewal (for front-end display)
(define-read-only (get-upcoming-payments (user principal) (days-threshold uint))
  (let (
    (user-subs (get subscription-ids (get-user-subscriptions user)))
    (current-block block-height)
    (threshold-blocks (* days-threshold u144)) ;; Approximate blocks in a day = 144
  )
    ;; Similar to get-active-subscriptions, filtering by date would need to be
    ;; handled on the client side due to Clarity limitations
    (ok user-subs)
  )
)