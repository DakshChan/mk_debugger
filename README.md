# Installation and setup

## Requirements
  You will to install the following:
   - [Nodejs](https://nodejs.org/en/) 
   - [Racket](https://racket-lang.org/)
   - [Git](https://git-scm.com/)

## Getting started
Clone the repository

    git clone https://github.com/DakshChan/mk_debugger

Install dependencies

    cd mk_debugger/backend
    npm install

Run the project

    npm start


## Development Guide
To start the development server for the frontend

Install dependencies

    cd mk_debugger/frontend
    npm install

Then start the development server

    npm start

The development server for the front end runs on port 3005
The backend runs on port 3000

## Building the frontend
Build the frontend

    npm run build

Copy the build folder into the backend folder

    cp -r ./build/ ../backend/

