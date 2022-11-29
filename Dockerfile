FROM ubuntu:latest
RUN apt update
RUN apt install -y sudo
RUN sudo apt install -y docker.io
RUN sudo apt install -y curl
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash - && sudo apt-get install -y nodejs
RUN sudo apt install -y software-properties-common
RUN sudo add-apt-repository ppa:plt/racket
RUN sudo apt-get install -y racket

WORKDIR /backend
COPY ./backend/package.json ./
RUN npm install

WORKDIR /frontend
COPY ./frontend/package.json ./
RUN npm install
COPY ./frontend .
RUN npm run build

WORKDIR /debugger
COPY ./debugger .

WORKDIR /backend
COPY ./backend .
RUN cp -r ../frontend/build .
CMD npm start