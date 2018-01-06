# New Linux box w/GPU

```
sudo yum install -y git emacs-nox wget curl bzip2 tmux

# NVIDIA driver

# Requirements
sudo yum install -y libglvnd-devel kernel-headers kernel-devel
sudo yum -y upgrade kernel kernel-devel
wget http://us.download.nvidia.com/XFree86/Linux-x86_64/384.98/NVIDIA-Linux-x86_64-384.98.run
chmod 755 NVIDIA-Linux-x86_64-384.98.run
sudo ./NVIDIA-Linux-x86_64-384.98.run

# Cuda
sudo yum install -y epel-release
sudo yum install -y dkms libffi-devel pciutils
sudo rpm install -y http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-repo-rhel7-9.1.85-1.x86_64.rpm
sudo yum clean all
sudo yum install -y cuda



# Docker https://docs.docker.com/engine/installation/linux/docker-ce/centos/
# Remove older Docker
sudo yum remove docker docker-common docker-selinux docker-engine

# New Docker repo
sudo yum install -y yum-utils device-mapper-persistent-data lvm2
sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
sudo yum install -y docker-ce

sudo systemctl start docker
sudo systemctl enable docker
sudo groupadd docker
sudo usermod -aG docker $USER

# NVIDIA Docker
curl -s -L https://nvidia.github.io/nvidia-docker/centos7/x86_64/nvidia-docker.repo | \
  sudo tee /etc/yum.repos.d/nvidia-docker.repo

# Install nvidia-docker2 and reload the Docker daemon configuration
sudo yum install -y nvidia-docker2
sudo pkill -SIGHUP dockerd

# Test nvidia-smi with the latest official CUDA image
docker run --runtime=nvidia --rm nvidia/cuda nvidia-smi

```

## Disable firewall

```
sudo systemctl stop firewalld
sudo systemctl disable firewalld
```
