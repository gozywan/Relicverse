;; Gaming Artifacts Marketplace - Epic Loot System
;; SIP-009 compliant gaming item marketplace with rarity levels

;; Define the gaming artifact token
(define-non-fungible-token epic-artifact uint)

;; Contract constants
(define-constant guild-master tx-sender)
(define-constant err-guild-master-only (err u200))
(define-constant err-not-artifact-owner (err u201))
(define-constant err-insufficient-gold (err u202))
(define-constant err-auction-not-found (err u203))
(define-constant err-artifact-not-found (err u204))
(define-constant err-invalid-bid (err u205))
(define-constant err-forging-failed (err u206))

;; Data variables
(define-data-var last-artifact-id uint u0)
(define-data-var treasury-vault (string-utf8 256) u"")

;; Rarity levels
(define-constant COMMON u1)
(define-constant RARE u2)
(define-constant EPIC u3)
(define-constant LEGENDARY u4)
(define-constant MYTHIC u5)

;; Data maps
(define-map artifact-stats uint {
  title: (string-utf8 64),
  lore: (string-utf8 256),
  avatar: (string-utf8 256),
  forger: principal,
  rarity: uint,
  power-level: uint
})

(define-map marketplace-auctions uint {
  seller: principal,
  gold-price: uint,
  is-active: bool
})

(define-map crafting-fees uint {
  forger: principal,
  cut: uint
})

;; Get artifact URI (SIP-009 requirement)
(define-read-only (get-token-uri (artifact-id uint))
  (ok (some (var-get treasury-vault)))
)

;; Get last artifact ID (SIP-009 requirement)
(define-read-only (get-last-token-id)
  (ok (var-get last-artifact-id))
)

;; Get artifact owner (SIP-009 requirement)
(define-read-only (get-owner (artifact-id uint))
  (ok (nft-get-owner? epic-artifact artifact-id))
)

;; Transfer function (SIP-009 requirement)
(define-public (transfer (artifact-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-artifact-owner)
    (nft-transfer? epic-artifact artifact-id sender recipient)
  )
)

;; Forge new artifact (guild master only)
(define-public (forge-artifact (champion principal) (title (string-utf8 64)) (lore (string-utf8 256)) (avatar (string-utf8 256)) (rarity uint))
  (let ((new-artifact-id (+ (var-get last-artifact-id) u1))
        (power-level (calculate-power-level rarity)))
    (asserts! (is-eq tx-sender guild-master) err-guild-master-only)
    (asserts! (<= rarity MYTHIC) err-invalid-bid)
    (asserts! (>= rarity COMMON) err-invalid-bid)
    (try! (nft-mint? epic-artifact new-artifact-id champion))
    (map-set artifact-stats new-artifact-id {
      title: title,
      lore: lore,
      avatar: avatar,
      forger: tx-sender,
      rarity: rarity,
      power-level: power-level
    })
    (map-set crafting-fees new-artifact-id {
      forger: tx-sender,
      cut: (get-crafting-percentage rarity)
    })
    (var-set last-artifact-id new-artifact-id)
    (ok new-artifact-id)
  )
)

;; Calculate power level based on rarity
(define-private (calculate-power-level (rarity uint))
  (if (is-eq rarity MYTHIC) u1000
    (if (is-eq rarity LEGENDARY) u750
      (if (is-eq rarity EPIC) u500
        (if (is-eq rarity RARE) u250
          u100))))
)

;; Get crafting percentage based on rarity
(define-private (get-crafting-percentage (rarity uint))
  (if (is-eq rarity MYTHIC) u10
    (if (is-eq rarity LEGENDARY) u8
      (if (is-eq rarity EPIC) u6
        (if (is-eq rarity RARE) u4
          u2))))
)

;; Get artifact stats
(define-read-only (get-artifact-stats (artifact-id uint))
  (map-get? artifact-stats artifact-id)
)

;; Put artifact up for auction
(define-public (start-auction (artifact-id uint) (gold-price uint))
  (let ((artifact-owner (unwrap! (nft-get-owner? epic-artifact artifact-id) err-artifact-not-found)))
    (asserts! (is-eq tx-sender artifact-owner) err-not-artifact-owner)
    (asserts! (> gold-price u0) err-invalid-bid)
    (try! (nft-transfer? epic-artifact artifact-id tx-sender (as-contract tx-sender)))
    (map-set marketplace-auctions artifact-id {
      seller: tx-sender,
      gold-price: gold-price,
      is-active: true
    })
    (ok true)
  )
)

;; Purchase artifact from auction
(define-public (purchase-artifact (artifact-id uint))
  (let ((auction (unwrap! (map-get? marketplace-auctions artifact-id) err-auction-not-found)))
    (asserts! (get is-active auction) err-auction-not-found)
    (asserts! (>= (stx-get-balance tx-sender) (get gold-price auction)) err-insufficient-gold)
    
    ;; Calculate crafting fees
    (let ((crafting-info (unwrap! (map-get? crafting-fees artifact-id) err-artifact-not-found))
          (sale-price (get gold-price auction))
          (crafting-fee (/ (* sale-price (get cut crafting-info)) u100))
          (seller-earnings (- sale-price crafting-fee)))
      
      ;; Transfer gold to seller
      (try! (stx-transfer? seller-earnings tx-sender (get seller auction)))
      
      ;; Transfer crafting fee to forger
      (try! (stx-transfer? crafting-fee tx-sender (get forger crafting-info)))
      
      ;; Transfer artifact to buyer
      (try! (nft-transfer? epic-artifact artifact-id (as-contract tx-sender) tx-sender))
      
      ;; Remove auction
      (map-delete marketplace-auctions artifact-id)
      (ok true)
    )
  )
)

;; Cancel auction
(define-public (cancel-auction (artifact-id uint))
  (let ((auction (unwrap! (map-get? marketplace-auctions artifact-id) err-auction-not-found)))
    (asserts! (is-eq tx-sender (get seller auction)) err-not-artifact-owner)
    (asserts! (get is-active auction) err-auction-not-found)
    (try! (nft-transfer? epic-artifact artifact-id (as-contract tx-sender) tx-sender))
    (map-delete marketplace-auctions artifact-id)
    (ok true)
  )
)

;; Get auction details
(define-read-only (get-auction-details (artifact-id uint))
  (map-get? marketplace-auctions artifact-id)
)

;; Get artifact market info
(define-read-only (get-market-info (artifact-id uint))
  (let ((auction (map-get? marketplace-auctions artifact-id))
        (stats (map-get? artifact-stats artifact-id)))
    {
      auction: auction,
      stats: stats
    }
  )
)

;; Update treasury vault URI (guild master only)
(define-public (update-treasury-vault (new-vault (string-utf8 256)))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-guild-master-only)
    (var-set treasury-vault new-vault)
    (ok true)
  )
)

;; Get treasury vault URI
(define-read-only (get-treasury-vault)
  (var-get treasury-vault)
)

;; Emergency artifact retrieval (guild master only)
(define-public (emergency-retrieve (artifact-id uint))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-guild-master-only)
    (nft-transfer? epic-artifact artifact-id (as-contract tx-sender) tx-sender)
  )
)

;; Batch forging
(define-public (batch-forge (champions (list 10 principal)) (titles (list 10 (string-utf8 64))) (lores (list 10 (string-utf8 256))) (avatars (list 10 (string-utf8 256))) (rarities (list 10 uint)))
  (begin
    (asserts! (is-eq tx-sender guild-master) err-guild-master-only)
    (asserts! (is-eq (len champions) (len titles)) err-invalid-bid)
    (asserts! (is-eq (len titles) (len lores)) err-invalid-bid)
    (asserts! (is-eq (len lores) (len avatars)) err-invalid-bid)
    (asserts! (is-eq (len avatars) (len rarities)) err-invalid-bid)
    (ok (map batch-forge-helper champions titles lores avatars rarities))
  )
)

(define-private (batch-forge-helper (champion principal) (title (string-utf8 64)) (lore (string-utf8 256)) (avatar (string-utf8 256)) (rarity uint))
  (forge-artifact champion title lore avatar rarity)
)

;; Utility functions
(define-read-only (get-total-artifacts)
  (var-get last-artifact-id)
)

(define-read-only (get-artifacts-by-champion (champion principal))
  (let ((total-artifacts (var-get last-artifact-id)))
    (fold check-champion-ownership (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list))
  )
)

(define-private (check-champion-ownership (artifact-id uint) (owned-list (list 10 uint)))
  (if (is-eq (nft-get-owner? epic-artifact artifact-id) (some tx-sender))
    (unwrap! (as-max-len? (append owned-list artifact-id) u10) owned-list)
    owned-list
  )
)

;; Get artifacts by rarity
(define-read-only (get-rarity-count (rarity uint))
  (let ((total-artifacts (var-get last-artifact-id)))
    (fold count-rarity-artifacts (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) {target: rarity, count: u0})
  )
)

(define-private (count-rarity-artifacts (artifact-id uint) (acc {target: uint, count: uint}))
  (let ((stats (map-get? artifact-stats artifact-id)))
    (match stats
      artifact-data 
        (if (is-eq (get rarity artifact-data) (get target acc))
          {target: (get target acc), count: (+ (get count acc) u1)}
          acc)
      acc
    )
  )
)