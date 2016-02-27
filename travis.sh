set -ex
sudo apt-get update

echo Install Shake
git clone https://github.com/ndmitchell/shake
(cd shake && cabal install)

echo Install Ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

echo Install tup
sudo apt-get install libfuse-dev
modprobe fuse
git clone https://github.com/gittup/tup
(cd tup && ./bootstrap.sh)
export PATH=$PATH:`pwd`/tup

echo Install fabricate
wget https://fabricate.googlecode.com/git/fabricate.py
export PYTHONPATH=$PWD:$PYTHONPATH

echo Install Aqualid
wget https://github.com/aqualid/aqualid/releases/download/v0.53-beta/Aqualid-0.53.tar.bz2
tar -xjf Aqualid-0.53.tar.bz2
export AQLPATH=$PWD/Aqualid
export PATH=$PATH:$AQLPATH
export PYTHONPATH=$PYTHONPATH:$AQLPATH
(cd Aqualid-0.53 && python setup.py install --install-lib $AQLPATH --install-headers $AQLPATH --install-scripts $AQLPATH --install-data $AQLPATH)

echo Install fbuild
sudo apt-get install python3 python3-setuptools
git clone https://github.com/felix-lang/fbuild.git
(cd fbuild && sudo python3 setup.py install)

export PATH=/home/travis/.ghc-multi/7.6.3/bin:$PATH
runhaskell Main shake make ninja fabricate aql fbuild tup
