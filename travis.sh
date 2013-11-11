# Install Shake
git clone https://github.com/ndmitchell/shake
(cd shake && cabal install)

# Install Ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

# Run the tests
runhaskell Main
