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
