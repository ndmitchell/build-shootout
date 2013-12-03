# Install Shake
git clone https://github.com/ndmitchell/shake
(cd shake && cabal install)

# Install Ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

# Install tup
# Does not work, see: https://github.com/travis-ci/travis-ci/issues/1100
# sudo apt-get install libfuse-dev
# modprobe fuse
# git clone https://github.com/gittup/tup
# (cd tup && ./bootstrap.sh)
# export PATH=$PATH:`pwd`/tup

# Install fabricate
wget https://fabricate.googlecode.com/git/fabricate.py
export PYTHONPATH=$PWD:$PYTHONPATH

export PATH=/home/travis/.ghc-multi/7.6.3/bin:$PATH
runhaskell Main shake make ninja fabricate
