;; CogniCraft - Decentralized Mentorship Platform

;; ========== Platform Configuration ==========
;; Base pricing in microstacks per hour of expertise
(define-data-var hourly-price-base uint u10)

;; Maximum expertise hours a single user can offer on the platform
(define-data-var user-expertise-cap uint u100)

;; Platform commission percentage taken from each transaction
(define-data-var platform-commission-rate uint u10)

;; Running total of all expertise hours currently available in the marketplace
(define-data-var marketplace-total-hours uint u0)

;; Global cap on total expertise hours available across all users
(define-data-var marketplace-capacity-limit uint u1000)

;; ========== User Storage Maps ==========
;; Tracks available expertise hours per user
(define-map user-expertise-inventory principal uint)

;; Tracks user's currency balance in the system
(define-map user-currency-inventory principal uint)

;; Tracks expertise hours that users have published for trading
(define-map published-expertise {owner: principal} {hours-available: uint, price-per-hour: uint})
;; ========== Constants ==========
(define-constant admin-address tx-sender)
(define-constant error-not-admin (err u200))
(define-constant error-balance-too-low (err u201))
(define-constant error-invalid-expertise-amount (err u202))
(define-constant error-invalid-pricing (err u203))
(define-constant error-global-capacity-exceeded (err u204))
(define-constant error-forbidden-operation (err u205))


;; ========== Premium Features Storage ==========
;; User verification status for premium offerings
(define-map verified-status principal bool)

;; Premium expertise offerings with verification status
(define-map premium-expertise-offerings {owner: principal} {hours-available: uint, price-per-hour: uint, is-verified: bool})

;; Reputation system tracking ratings between users
(define-map expertise-ratings {provider: principal, reviewer: principal} uint)

;; Aggregated reputation scores
(define-map reputation-scores principal {cumulative-score: uint, total-reviews: uint})

;; Discounted expertise packages
(define-map expertise-packages {owner: principal} {hours-available: uint, price-per-hour: uint, discount-percent: uint})

;; Collaborative expertise sessions
(define-map collaborative-sessions uint {organizer: principal, members: (list 10 principal), hours-per-member: uint, price-per-hour: uint, current-status: (string-ascii 20)})

;; Counter for unique session identifiers
(define-data-var session-counter uint u0)

;; ========== Private Helper Functions ==========

;; Calculate the platform's commission amount from a transaction
(define-private (determine-commission-amount (transaction-value uint))
  (/ (* transaction-value (var-get platform-commission-rate)) u100))

;; Update the marketplace's available expertise hours
(define-private (adjust-marketplace-hours (hour-adjustment int))
  (let (
    (current-hours (var-get marketplace-total-hours))
    (updated-hours (if (< hour-adjustment 0)
                     (if (>= current-hours (to-uint (- 0 hour-adjustment)))
                         (- current-hours (to-uint (- 0 hour-adjustment)))
                         u0)
                     (+ current-hours (to-uint hour-adjustment))))
  )
    (asserts! (<= updated-hours (var-get marketplace-capacity-limit)) error-global-capacity-exceeded)
    (var-set marketplace-total-hours updated-hours)
    (ok true)))

;; ========== Public Marketplace Functions ==========

;; Register new expertise hours in user's account
(define-public (acquire-expertise-hours (hours uint))
  (let (
    (user tx-sender)
    (current-hours (default-to u0 (map-get? user-expertise-inventory user)))
    (user-limit (var-get user-expertise-cap))
    (acquisition-cost (* hours (var-get hourly-price-base)))
    (user-currency (default-to u0 (map-get? user-currency-inventory user)))
  )
    ;; Validate inputs and balances
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (<= (+ current-hours hours) user-limit) (err u206))
    (asserts! (>= user-currency acquisition-cost) error-balance-too-low)

    ;; Update user account
    (map-set user-expertise-inventory user (+ current-hours hours))
    (map-set user-currency-inventory user (- user-currency acquisition-cost))

    ;; Transfer payment to admin
    (map-set user-currency-inventory admin-address 
             (+ (default-to u0 (map-get? user-currency-inventory admin-address)) acquisition-cost))

    (ok true)))

;; Publish expertise hours for others to acquire
(define-public (publish-expertise-offering (hours uint) (hourly-rate uint))
  (let (
    (current-hours (default-to u0 (map-get? user-expertise-inventory tx-sender)))
    (current-published (get hours-available (default-to {hours-available: u0, price-per-hour: u0} 
                                            (map-get? published-expertise {owner: tx-sender}))))
    (new-published-total (+ hours current-published))
  )
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (> hourly-rate u0) error-invalid-pricing)
    (asserts! (>= current-hours new-published-total) error-balance-too-low)
    (try! (adjust-marketplace-hours (to-int hours)))
    (map-set published-expertise {owner: tx-sender} 
             {hours-available: new-published-total, price-per-hour: hourly-rate})
    (ok true)))

;; Purchase expertise hours from another user
(define-public (purchase-expertise (provider principal) (hours uint))
  (let (
    (offering-details (default-to {hours-available: u0, price-per-hour: u0} 
                      (map-get? published-expertise {owner: provider})))
    (transaction-value (* hours (get price-per-hour offering-details)))
    (commission-fee (determine-commission-amount transaction-value))
    (total-cost (+ transaction-value commission-fee))
    (provider-available-hours (default-to u0 (map-get? user-expertise-inventory provider)))
    (buyer-currency (default-to u0 (map-get? user-currency-inventory tx-sender)))
    (provider-currency (default-to u0 (map-get? user-currency-inventory provider)))
  )
    ;; Validate transaction prerequisites
    (asserts! (not (is-eq tx-sender provider)) error-forbidden-operation)
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (>= (get hours-available offering-details) hours) error-balance-too-low)
    (asserts! (>= provider-available-hours hours) error-balance-too-low)
    (asserts! (>= buyer-currency total-cost) error-balance-too-low)

    ;; Update provider's expertise balances
    (map-set user-expertise-inventory provider (- provider-available-hours hours))
    (map-set published-expertise {owner: provider} 
             {hours-available: (- (get hours-available offering-details) hours), 
              price-per-hour: (get price-per-hour offering-details)})

    ;; Update buyer's balances
    (map-set user-currency-inventory tx-sender (- buyer-currency total-cost))
    (map-set user-expertise-inventory tx-sender 
             (+ (default-to u0 (map-get? user-expertise-inventory tx-sender)) hours))

    ;; Distribute payment and commission
    (map-set user-currency-inventory provider (+ provider-currency transaction-value))
    (map-set user-currency-inventory admin-address 
             (+ (default-to u0 (map-get? user-currency-inventory admin-address)) commission-fee))

    (ok true)))

;; Deposit currency into platform wallet
(define-public (deposit-currency (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-currency-inventory tx-sender)))
    (updated-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) (err u206))
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update internal balance
    (map-set user-currency-inventory tx-sender updated-balance)
    (ok true)))

;; Withdraw currency from platform wallet
(define-public (withdraw-currency (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-currency-inventory tx-sender)))
    (contract-funds (as-contract (stx-get-balance tx-sender)))
  )
    (asserts! (> amount u0) (err u206))
    (asserts! (>= current-balance amount) error-balance-too-low)
    (asserts! (>= contract-funds amount) error-balance-too-low)

    ;; Execute the transfer
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update internal balance
    (map-set user-currency-inventory tx-sender (- current-balance amount))

    (ok true)))

;; ========== Premium Functions ==========

;; Publish premium verified expertise offerings
(define-public (publish-premium-expertise (hours uint) (hourly-rate uint))
  (let (
    (current-hours (default-to u0 (map-get? user-expertise-inventory tx-sender)))
    (user-verified (default-to false (map-get? verified-status tx-sender)))
    (current-published (get hours-available (default-to {hours-available: u0, price-per-hour: u0} 
                                            (map-get? published-expertise {owner: tx-sender}))))
    (new-published-total (+ hours current-published))
  )
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (> hourly-rate u0) error-invalid-pricing)
    (asserts! user-verified (err u211))
    (asserts! (>= current-hours new-published-total) error-balance-too-low)
    (try! (adjust-marketplace-hours (to-int hours)))

    ;; Update standard offerings
    (map-set published-expertise {owner: tx-sender} 
             {hours-available: new-published-total, price-per-hour: hourly-rate})

    ;; Add to premium offerings
    (map-set premium-expertise-offerings {owner: tx-sender} 
             {hours-available: hours, price-per-hour: hourly-rate, is-verified: true})

    (ok true)))

;; Submit a rating for an expertise provider
(define-public (submit-provider-rating (provider principal) (rating uint))
  (let (
    (provider-reputation (default-to {cumulative-score: u0, total-reviews: u0} 
                         (map-get? reputation-scores provider)))
    (current-score (get cumulative-score provider-reputation))
    (current-review-count (get total-reviews provider-reputation))
    (updated-score (+ current-score rating))
    (updated-review-count (+ current-review-count u1))
  )
    (asserts! (not (is-eq tx-sender provider)) error-forbidden-operation)
    (asserts! (>= rating u1) (err u212))
    (asserts! (<= rating u5) (err u213))

    ;; Store individual rating
    (map-set expertise-ratings {provider: provider, reviewer: tx-sender} rating)

    ;; Update aggregated reputation
    (map-set reputation-scores provider 
             {cumulative-score: updated-score, total-reviews: updated-review-count})

    (ok true)))

;; Create discounted expertise package
(define-public (create-expertise-package (hours uint) (hourly-rate uint) (discount-percent uint))
  (let (
    (current-hours (default-to u0 (map-get? user-expertise-inventory tx-sender)))
    (current-published (get hours-available (default-to {hours-available: u0, price-per-hour: u0} 
                                            (map-get? published-expertise {owner: tx-sender}))))
    (current-package (default-to {hours-available: u0, price-per-hour: u0, discount-percent: u0} 
                     (map-get? expertise-packages {owner: tx-sender})))
    (new-published-total (+ hours current-published))
    (total-package-hours (+ hours (get hours-available current-package)))
  )
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (> hourly-rate u0) error-invalid-pricing)
    (asserts! (> discount-percent u0) (err u214))
    (asserts! (<= discount-percent u50) (err u215))
    (asserts! (>= current-hours new-published-total) error-balance-too-low)

    ;; Update marketplace hours
    (try! (adjust-marketplace-hours (to-int hours)))

    ;; Update published offerings
    (map-set published-expertise {owner: tx-sender} 
             {hours-available: new-published-total, price-per-hour: hourly-rate})

    ;; Create/update package offering
    (map-set expertise-packages {owner: tx-sender} 
             {hours-available: total-package-hours, 
              price-per-hour: hourly-rate, 
              discount-percent: discount-percent})

    (ok true)))

;; Create collaborative expertise session
(define-public (create-collaborative-session (participants (list 10 principal)) (hours-per-member uint) (hourly-rate uint))
  (let (
    (organizer-hours (default-to u0 (map-get? user-expertise-inventory tx-sender)))
    (session-id (var-get session-counter))
    (participant-count (len participants))
    (total-required-hours (* hours-per-member participant-count))
  )
    (asserts! (> hours-per-member u0) error-invalid-expertise-amount)
    (asserts! (> hourly-rate u0) error-invalid-pricing)
    (asserts! (>= organizer-hours total-required-hours) error-balance-too-low)

    ;; Update marketplace hours
    (try! (adjust-marketplace-hours (to-int total-required-hours)))

    ;; Update organizer's expertise balance
    (map-set user-expertise-inventory tx-sender (- organizer-hours total-required-hours))

    ;; Increment session counter
    (var-set session-counter (+ session-id u1))

    (ok session-id)))

;; Reclaim unpurchased expertise hours from marketplace
(define-public (reclaim-published-expertise (hours uint))
  (let (
    (offering-details (default-to {hours-available: u0, price-per-hour: u0} 
                      (map-get? published-expertise {owner: tx-sender})))
    (available-published-hours (get hours-available offering-details))
    (user-hours (default-to u0 (map-get? user-expertise-inventory tx-sender)))
  )
    (asserts! (> hours u0) error-invalid-expertise-amount)
    (asserts! (>= available-published-hours hours) error-balance-too-low)

    ;; Update published offerings
    (map-set published-expertise {owner: tx-sender} 
             {hours-available: (- available-published-hours hours),
              price-per-hour: (get price-per-hour offering-details)})

    ;; Return hours to user's inventory
    (map-set user-expertise-inventory tx-sender user-hours)

    ;; Handle premium offerings if applicable
    (if (is-some (map-get? premium-expertise-offerings {owner: tx-sender}))
        (let (
          (premium-details (unwrap-panic (map-get? premium-expertise-offerings {owner: tx-sender})))
          (premium-hours (get hours-available premium-details))
        )
          (if (>= premium-hours hours)
              (map-set premium-expertise-offerings {owner: tx-sender} 
                       {hours-available: (- premium-hours hours),
                        price-per-hour: (get price-per-hour premium-details),
                        is-verified: (get is-verified premium-details)})
              (map-delete premium-expertise-offerings {owner: tx-sender})
          )
        )
        true
    )

    (ok true)))

;; ========== Administrative Functions ==========

;; Update platform configuration parameters
(define-public (update-platform-configuration (new-hourly-base uint) 
                                             (new-commission-rate uint) 
                                             (new-user-cap uint) 
                                             (new-marketplace-limit uint))
  (begin
    (asserts! (is-eq tx-sender admin-address) error-not-admin)
    (asserts! (> new-hourly-base u0) error-invalid-pricing)
    (asserts! (<= new-commission-rate u30) (err u208))
    (asserts! (> new-user-cap u0) (err u209))
    (asserts! (>= new-marketplace-limit (var-get marketplace-total-hours)) (err u210))

    ;; Update all configuration parameters
    (var-set hourly-price-base new-hourly-base)
    (var-set platform-commission-rate new-commission-rate)
    (var-set user-expertise-cap new-user-cap)
    (var-set marketplace-capacity-limit new-marketplace-limit)

    (ok true)))

