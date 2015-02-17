set -ex

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

wget https://github.com/aqualid/aqualid/releases/download/v0.53-beta/Aqualid-0.53.tar.bz2
tar -xjf Aqualid-0.53.tar.bz2
(cd Aqualid-0.53 && python setup.py install)

export PATH=/home/travis/.ghc-multi/7.6.3/bin:$PATH
runhaskell Main shake make ninja fabricate aql --continue

runhaskell Main monad1 fabricate --verbose --continue
