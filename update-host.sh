#!/bin/sh

ssh ec2.untroubled.be <<EOF
cd /srv/rlog
git pull
EOF
