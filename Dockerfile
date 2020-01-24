FROM registry.gitlab.com/couchbits/movestore/movestore-groundcontrol/movestore-apps/pilot-base

# Install R dependencies
RUN R -e "install.packages(c('foreach'), repos='https://cloud.r-project.org/')"

COPY RFunction.R /root/app/r/RFunction.R
