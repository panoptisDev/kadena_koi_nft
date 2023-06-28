(module kadena_koi_fish_nft
    (use std)
    (use std.signer)
    (use std.vector)
    (use std.aptos_coin.AptosCoin)
    (use aptos_token.token)
  
    ;; Constants
    (defconst CONTRACT_NAME "KadenaKoiFish")
    (defconst TOKEN_NAME "KoiFish")
    (defconst TOKEN_URI "https://example.com/koi-fish/")
    (defconst MINT_PRICE 1000000000)  ; 1 Kadena
  
    ;; Public functions
  
    (defun mint_nft (buyer: &signer)
      (defvar buyer_addr (signer.address_of buyer))
      (coin.transfer AptosCoin buyer_addr @contract MINT_PRICE)
      
      (defvar token_data_id (token.mint_token buyer_addr TOKEN_NAME TOKEN_URI))
      
      (format "NFT minted successfully. Token ID: {token_data_id}")
    )
  )