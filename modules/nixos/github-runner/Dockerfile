# GitHub Runner with PHP 8.2 for x86_64 Linux
# Based on Ubuntu 22.04 with Git LFS support

FROM ubuntu:22.04

# Prevent interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install basic dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    git-lfs \
    jq \
    wget \
    unzip \
    zip \
    sudo \
    ca-certificates \
    gnupg \
    lsb-release \
    software-properties-common \
    apt-transport-https

# Initialize git-lfs
RUN git lfs install --system

# Add PHP repository and install PHP 8.2
RUN add-apt-repository -y ppa:ondrej/php && \
    apt-get update && \
    apt-get install -y \
    php8.2 \
    php8.2-cli \
    php8.2-common \
    php8.2-mysql \
    php8.2-zip \
    php8.2-gd \
    php8.2-mbstring \
    php8.2-curl \
    php8.2-xml \
    php8.2-bcmath \
    php8.2-intl \
    php8.2-soap \
    php8.2-sqlite3 \
    php8.2-pgsql \
    php8.2-redis \
    php8.2-xdebug \
    php8.2-dev

# Install Composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer

# Install Node.js 20.x
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g yarn

# Install Docker CLI
RUN curl -fsSL https://get.docker.com | sh

# Install database clients
RUN apt-get install -y \
    mysql-client \
    postgresql-client \
    redis-tools

# Install Playwright dependencies
# These are all the packages that were being installed during the test run
RUN apt-get update && apt-get install -y \
    # Core browser dependencies
    xvfb \
    fonts-ipafont-gothic \
    libfribidi0 \
    xkb-data \
    libdrm-common \
    libdrm2 \
    fontconfig \
    fonts-freefont-ttf \
    fonts-liberation \
    fonts-noto-color-emoji \
    fonts-tlwg-loma-otf \
    fonts-unifont \
    fonts-wqy-zenhei \
    libasound2-data \
    libasound2 \
    libatk1.0-data \
    libatk1.0-0 \
    libxi6 \
    libatspi2.0-0 \
    libatk-bridge2.0-0 \
    libavahi-common-data \
    libavahi-common3 \
    libavahi-client3 \
    libpixman-1-0 \
    libxcb-render0 \
    libxcb-shm0 \
    libxrender1 \
    libcairo2 \
    libcups2 \
    libdatrie1 \
    libdrm-amdgpu1 \
    libpciaccess0 \
    libdrm-intel1 \
    libdrm-nouveau2 \
    libdrm-radeon1 \
    libfontenc1 \
    libwayland-server0 \
    libxcb-randr0 \
    libgbm1 \
    libglapi-mesa \
    libllvm15 \
    libsensors-config \
    libsensors5 \
    libxcb-dri3-0 \
    libgl1-mesa-dri \
    libx11-xcb1 \
    libxcb-dri2-0 \
    libxcb-glx0 \
    libxcb-present0 \
    libxcb-sync1 \
    libxcb-xfixes0 \
    libxfixes3 \
    libxshmfence1 \
    libxxf86vm1 \
    libglx-mesa0 \
    libgraphite2-3 \
    libharfbuzz0b \
    x11-common \
    libice6 \
    libnspr4 \
    libnss3 \
    libthai-data \
    libthai0 \
    libpango-1.0-0 \
    libsm6 \
    libwayland-client0 \
    libxt6 \
    libxmu6 \
    libxaw7 \
    libxcomposite1 \
    libxdamage1 \
    libxfont2 \
    libxkbcommon0 \
    libxkbfile1 \
    libxrandr2 \
    x11-xkb-utils \
    xfonts-encodings \
    xfonts-utils \
    xfonts-cyrillic \
    xfonts-scalable \
    xserver-common \
    libglvnd0 \
    libglx0 \
    libgl1 \
    # Additional dependencies for Playwright
    libgtk-3-0 \
    libgdk-pixbuf-2.0-0 \
    libdbus-glib-1-2 \
    libxss1 \
    libegl1 \
    libgles2 \
    libnotify4 \
    libxtst6

# Create runner user
RUN useradd -m -s /bin/bash runner && \
    usermod -aG sudo,docker runner && \
    echo "runner ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Pre-install Playwright browsers as the runner user
# This will download the browsers during the image build instead of during test runs
USER runner
WORKDIR /home/runner
RUN npx playwright install chromium firefox webkit

# Switch back to root to install system dependencies
USER root
RUN npx playwright install-deps

# Install GitHub Runner for x86_64
RUN cd /home/runner && \
    RUNNER_VERSION=$(curl -s https://api.github.com/repos/actions/runner/releases/latest | grep tag_name | cut -d '"' -f 4 | sed 's/^v//') && \
    curl -L -O https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz && \
    tar xzf actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz && \
    rm actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz && \
    chown -R runner:runner /home/runner && \
    /home/runner/bin/installdependencies.sh

# Clean up
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Create directories
RUN mkdir -p /home/runner/.composer && \
    chown -R runner:runner /home/runner/.composer

# Switch to runner user
USER runner
WORKDIR /home/runner

# Set environment variables
ENV COMPOSER_HOME=/home/runner/.composer
ENV PATH="/home/runner/.composer/vendor/bin:${PATH}"

# Create entrypoint script
RUN echo '#!/bin/bash\n\
set -e\n\
\n\
echo "Starting GitHub runner..."\n\
echo "Runner: ${RUNNER_NAME}"\n\
echo "Scope: ${RUNNER_SCOPE}"\n\
\n\
# Check Docker access\n\
if ! docker version > /dev/null 2>&1; then\n\
  echo "ERROR: Cannot access Docker daemon"\n\
  echo "Trying to fix permissions..."\n\
  sudo chmod 666 /var/run/docker.sock || true\n\
  if ! docker version > /dev/null 2>&1; then\n\
    echo "Still cannot access Docker. Exiting."\n\
    exit 1\n\
  fi\n\
fi\n\
\n\
if [ -z "${RUNNER_NAME}" ]; then\n\
  echo "RUNNER_NAME environment variable is not set"\n\
  exit 1\n\
fi\n\
\n\
if [ -z "${ACCESS_TOKEN}" ]; then\n\
  echo "ACCESS_TOKEN environment variable is not set"\n\
  exit 1\n\
fi\n\
\n\
# Configure based on scope\n\
if [ "${RUNNER_SCOPE}" = "org" ]; then\n\
  if [ -z "${ORG_NAME}" ]; then\n\
    echo "ORG_NAME required for org runners"\n\
    exit 1\n\
  fi\n\
  echo "Configuring runner for organization ${ORG_NAME}"\n\
  RUNNER_URL="https://github.com/${ORG_NAME}"\n\
  RUNNER_TOKEN=$(curl -sX POST -H "Authorization: token ${ACCESS_TOKEN}" \\\n\
    https://api.github.com/orgs/${ORG_NAME}/actions/runners/registration-token | jq -r .token)\n\
else\n\
  if [ -z "${REPO_URL}" ]; then\n\
    echo "REPO_URL required for repo runners"\n\
    exit 1\n\
  fi\n\
  echo "Configuring runner for repository ${REPO_URL}"\n\
  RUNNER_URL="${REPO_URL}"\n\
  REPO_PATH=$(echo ${REPO_URL} | sed "s|https://github.com/||")\n\
  RUNNER_TOKEN=$(curl -sX POST -H "Authorization: token ${ACCESS_TOKEN}" \\\n\
    https://api.github.com/repos/${REPO_PATH}/actions/runners/registration-token | jq -r .token)\n\
fi\n\
\n\
# Configure the runner\n\
./config.sh \\\n\
  --url "${RUNNER_URL}" \\\n\
  --token "${RUNNER_TOKEN}" \\\n\
  --name "${RUNNER_NAME}" \\\n\
  --labels "${LABELS:-self-hosted,linux,x64,php82}" \\\n\
  --work "${RUNNER_WORKDIR:-_work}" \\\n\
  --unattended \\\n\
  --replace\n\
\n\
# Cleanup function\n\
cleanup() {\n\
  echo "Removing runner..."\n\
  ./config.sh remove --unattended --token "${RUNNER_TOKEN}"\n\
}\n\
\n\
trap cleanup EXIT\n\
\n\
# Run the runner\n\
./run.sh\n\
' > /home/runner/entrypoint.sh && chmod +x /home/runner/entrypoint.sh

ENTRYPOINT ["/home/runner/entrypoint.sh"]
