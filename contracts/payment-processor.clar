;; payment-processor.clar
;;
;; This contract implements a flexible recurring payment processor for subscriptions
;; on the Stacks blockchain. It enables automated handling of subscription payments,
;; interacts with the subscription manager contract, and provides various payment
;; options and safeguards to users.
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SUBSCRIPTION (err u101))
(define-constant ERR-PAYMENT-FAILED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-AMOUNT-EXCEEDS-THRESHOLD (err u104))
(define-constant ERR-APPROVAL-REQUIRED (err u105))
(define-constant ERR-ALREADY-PAID (err u106))
(define-constant ERR-INVALID-TOKEN (err u107))
(define-constant ERR-INVALID-AMOUNT (err u108))
(define-constant ERR-PAYMENT-NOT-DUE (err u109))
(define-constant ERR-AUTO-PAYMENT-DISABLED (err u110))
(define-constant ERR-INVALID-PARAMETER (err u111))
;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant PAYMENT-PRECISION u1000000) ;; 6 decimal places for payment amounts
;; Data maps and variables
;; Track all payments made
(define-map payments
  {
    payment-id: uint,
    subscription-id: uint,
  }
  {
    payer: principal,
    recipient: principal,
    amount: uint,
    token-contract: (optional principal),
    paid-at-block: uint,
    status: (string-ascii 20), ;; "completed", "failed", "refunded"
  }
)
;; Store auto-payment configurations for users
(define-map auto-payment-settings
  { user: principal }
  {
    enabled: bool,
    max-payment-threshold: uint, ;; Maximum amount that can be paid automatically
    requires-approval-above-threshold: bool,
  }
)
;; Store payment approvals for amounts above thresholds
(define-map pending-approvals
  {
    subscription-id: uint,
    payment-id: uint,
  }
  {
    user: principal,
    amount: uint,
    due-date: uint,
    approved: bool,
  }
)
;; Track all registered subscription services
(define-map subscription-services
  { service-id: uint }
  {
    name: (string-ascii 64),
    owner: principal,
    active: bool,
  }
)
;; Track active subscriptions
(define-map active-subscriptions
  { subscription-id: uint }
  {
    service-id: uint,
    subscriber: principal,
    recipient: principal,
    payment-amount: uint,
    payment-period: uint, ;; in blocks
    next-payment-block: uint,
    payment-count: uint,
    token-contract: (optional principal), ;; None for STX
  }
)
;; Track the next available IDs
(define-data-var next-payment-id uint u1)
(define-data-var next-subscription-id uint u1)
(define-data-var next-service-id uint u1)
;; Private functions
;; Get the current payment ID and increment for next use
(define-private (generate-payment-id)
  (let ((current-id (var-get next-payment-id)))
    (var-set next-payment-id (+ current-id u1))
    current-id
  )
)

;; Get the current subscription ID and increment for next use
(define-private (generate-subscription-id)
  (let ((current-id (var-get next-subscription-id)))
    (var-set next-subscription-id (+ current-id u1))
    current-id
  )
)

;; Get the current service ID and increment for next use
(define-private (generate-service-id)
  (let ((current-id (var-get next-service-id)))
    (var-set next-service-id (+ current-id u1))
    current-id
  )
)

;; Process STX payment
(define-private (process-stx-payment
    (recipient principal)
    (amount uint)
  )
  (if (>= (stx-get-balance tx-sender) amount)
    (stx-transfer? amount tx-sender recipient)
    ERR-INSUFFICIENT-FUNDS
  )
)

;; Check if payment is under the user's threshold or requires approval
(define-private (check-payment-threshold
    (user principal)
    (amount uint)
  )
  (let ((settings (default-to {
      enabled: false,
      max-payment-threshold: u0,
      requires-approval-above-threshold: true,
    }
      (map-get? auto-payment-settings { user: user })
    )))
    (if (> amount (get max-payment-threshold settings))
      (if (get requires-approval-above-threshold settings)
        ERR-APPROVAL-REQUIRED
        (ok true)
      )
      (ok true)
    )
  )
)

;; Check if a subscription is valid and due for payment
(define-private (is-payment-due (subscription-id uint))
  (match (map-get? active-subscriptions { subscription-id: subscription-id })
    subscription (if (>= block-height (get next-payment-block subscription))
      (ok true)
      ERR-PAYMENT-NOT-DUE
    )
    ERR-INVALID-SUBSCRIPTION
  )
)

;; Update subscription after successful payment
(define-private (update-subscription-after-payment (subscription-id uint))
  (match (map-get? active-subscriptions { subscription-id: subscription-id })
    subscription (begin
      (map-set active-subscriptions { subscription-id: subscription-id }
        (merge subscription {
          next-payment-block: (+ (get next-payment-block subscription)
            (get payment-period subscription)
          ),
          payment-count: (+ (get payment-count subscription) u1),
        })
      )
      (ok true)
    )
    ERR-INVALID-SUBSCRIPTION
  )
)

;; Record a payment in the payments map
(define-private (record-payment
    (subscription-id uint)
    (status (string-ascii 20))
  )
  (let (
      (payment-id (generate-payment-id))
      (subscription (unwrap!
        (map-get? active-subscriptions { subscription-id: subscription-id })
        ERR-INVALID-SUBSCRIPTION
      ))
    )
    (map-set payments {
      payment-id: payment-id,
      subscription-id: subscription-id,
    } {
      payer: (get subscriber subscription),
      recipient: (get recipient subscription),
      amount: (get payment-amount subscription),
      token-contract: (get token-contract subscription),
      paid-at-block: block-height,
      status: status,
    })
    (ok payment-id)
  )
)

;; Read-only functions
;; Get auto-payment settings for a user
(define-read-only (get-auto-payment-settings (user principal))
  (default-to {
    enabled: false,
    max-payment-threshold: u0,
    requires-approval-above-threshold: true,
  }
    (map-get? auto-payment-settings { user: user })
  )
)

;; Get payment details
(define-read-only (get-payment-details
    (payment-id uint)
    (subscription-id uint)
  )
  (map-get? payments {
    payment-id: payment-id,
    subscription-id: subscription-id,
  })
)

;; Get subscription details
(define-read-only (get-subscription (subscription-id uint))
  (map-get? active-subscriptions { subscription-id: subscription-id })
)

;; Get service details
(define-read-only (get-service (service-id uint))
  (map-get? subscription-services { service-id: service-id })
)

;; Check if a payment needs approval
(define-read-only (needs-approval (subscription-id uint))
  (match (map-get? active-subscriptions { subscription-id: subscription-id })
    subscription (let (
        (settings (get-auto-payment-settings (get subscriber subscription)))
        (payment-amount (get payment-amount subscription))
      )
      (and
        (get enabled settings)
        (> payment-amount (get max-payment-threshold settings))
        (get requires-approval-above-threshold settings)
      )
    )
    false
  )
)

;; Get pending approval status
(define-read-only (get-pending-approval
    (subscription-id uint)
    (payment-id uint)
  )
  (map-get? pending-approvals {
    subscription-id: subscription-id,
    payment-id: payment-id,
  })
)

;; Public functions
;; Register a new subscription service
(define-public (register-service (name (string-ascii 64)))
  (let ((service-id (generate-service-id)))
    (map-set subscription-services { service-id: service-id } {
      name: name,
      owner: tx-sender,
      active: true,
    })
    (ok service-id)
  )
)

;; Create a new subscription
(define-public (create-subscription
    (service-id uint)
    (recipient principal)
    (payment-amount uint)
    (payment-period uint)
    (token-contract (optional principal))
  )
  (let (
      (subscription-id (generate-subscription-id))
      (service (default-to {
        name: "",
        owner: tx-sender,
        active: false,
      }
        (map-get? subscription-services { service-id: service-id })
      ))
    )
    ;; Check that service exists and is active
    (asserts! (get active service) ERR-INVALID-SUBSCRIPTION)
    ;; Check valid parameters
    (asserts! (> payment-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> payment-period u0) ERR-INVALID-PARAMETER)
    ;; Create the subscription
    (map-set active-subscriptions { subscription-id: subscription-id } {
      service-id: service-id,
      subscriber: tx-sender,
      recipient: recipient,
      payment-amount: payment-amount,
      payment-period: payment-period,
      next-payment-block: (+ block-height payment-period),
      payment-count: u0,
      token-contract: token-contract,
    })
    (ok subscription-id)
  )
)

;; Configure auto-payment settings
(define-public (configure-auto-payment
    (enabled bool)
    (max-payment-threshold uint)
    (requires-approval-above-threshold bool)
  )
  (begin
    (map-set auto-payment-settings { user: tx-sender } {
      enabled: enabled,
      max-payment-threshold: max-payment-threshold,
      requires-approval-above-threshold: requires-approval-above-threshold,
    })
    (ok true)
  )
)

;; Request approval for payments above threshold
(define-public (request-payment-approval (subscription-id uint))
  (let (
      (subscription (unwrap!
        (map-get? active-subscriptions { subscription-id: subscription-id })
        ERR-INVALID-SUBSCRIPTION
      ))
      (user (get subscriber subscription))
      (payment-id (generate-payment-id))
    )
    ;; Check if payment requires approval based on settings and amount
    (asserts! (needs-approval subscription-id) ERR-APPROVAL-REQUIRED)
    ;; Create pending approval
    (map-set pending-approvals {
      subscription-id: subscription-id,
      payment-id: payment-id,
    } {
      user: user,
      amount: (get payment-amount subscription),
      due-date: (get next-payment-block subscription),
      approved: false,
    })
    (ok payment-id)
  )
)

;; Cancel a subscription
(define-public (cancel-subscription (subscription-id uint))
  (let ((subscription (unwrap! (map-get? active-subscriptions { subscription-id: subscription-id })
      ERR-INVALID-SUBSCRIPTION
    )))
    ;; Verify the caller is the subscriber
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR-NOT-AUTHORIZED)
    ;; Delete the subscription
    (map-delete active-subscriptions { subscription-id: subscription-id })
    (ok true)
  )
)

;; Update a subscription's payment amount
(define-public (update-subscription-amount
    (subscription-id uint)
    (new-payment-amount uint)
  )
  (let ((subscription (unwrap! (map-get? active-subscriptions { subscription-id: subscription-id })
      ERR-INVALID-SUBSCRIPTION
    )))
    ;; Verify the caller is the subscriber
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR-NOT-AUTHORIZED)
    ;; Check valid amount
    (asserts! (> new-payment-amount u0) ERR-INVALID-AMOUNT)
    ;; Update the subscription
    (map-set active-subscriptions { subscription-id: subscription-id }
      (merge subscription { payment-amount: new-payment-amount })
    )
    (ok true)
  )
)

;; Update a subscription's payment period
(define-public (update-subscription-period
    (subscription-id uint)
    (new-payment-period uint)
  )
  (let ((subscription (unwrap! (map-get? active-subscriptions { subscription-id: subscription-id })
      ERR-INVALID-SUBSCRIPTION
    )))
    ;; Verify the caller is the subscriber
    (asserts! (is-eq tx-sender (get subscriber subscription)) ERR-NOT-AUTHORIZED)
    ;; Check valid period
    (asserts! (> new-payment-period u0) ERR-INVALID-PARAMETER)
    ;; Update the subscription
    (map-set active-subscriptions { subscription-id: subscription-id }
      (merge subscription {
        payment-period: new-payment-period,
        next-payment-block: (+ block-height new-payment-period),
      })
    )
    (ok true)
  )
)
