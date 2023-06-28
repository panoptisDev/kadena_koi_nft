import React, { useState } from 'react';
import { Pact } from 'pact-lang-api';
import WalletConnectProvider from '@walletconnect/web3-provider';

const CONTRACT_NAME = 'KadenaKoiFish';
const NODE_API = 'http://localhost:9001'; // Replace with your Pact node API endpoint

function KoiFishNFTMinter() {
  const [mintingStatus, setMintingStatus] = useState('');
  const [loading, setLoading] = useState(false);
  const [pact, setPact] = useState(null);

  const handleWalletConnect = async () => {
    const provider = new WalletConnectProvider({
      rpc: { '1': NODE_API },
      chainId: 1
    });

    try {
      await provider.enable();
      const accounts = await provider.request({ method: 'eth_accounts' });
      const publicKey = accounts[0];

      const buyerKeyPair = {
        publicKey,
        provider
      };

      const newPact = new Pact({ provider, keyPairs: [buyerKeyPair] });
      setPact(newPact);
    } catch (error) {
      console.error('Error connecting wallet:', error);
    }
  };

  const handleMintClick = async () => {
    setLoading(true);
    setMintingStatus('');

    try {
      if (!pact) {
        throw new Error('Wallet not connected.');
      }

      const result = await pact.simple.exec(CONTRACT_NAME, 'mint_nft', { buyer: pact.wallet.balances[0].guard });

      setMintingStatus(`NFT minted successfully. Token ID: ${result.data}`);
    } catch (error) {
      console.error('Error minting NFT:', error);
      setMintingStatus('Error minting NFT. Please try again.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div>
      <button onClick={handleWalletConnect} disabled={pact !== null}>
        Connect Wallet
      </button>
      <button onClick={handleMintClick} disabled={!pact || loading}>
        {loading ? 'Minting...' : 'Mint NFT'}
      </button>
      <p>{mintingStatus}</p>
    </div>
  );
}
