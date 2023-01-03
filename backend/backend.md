# Backend Notes

The backend is written in JavaScript using the [Express](https://expressjs.com/) framework.
The backend is responsible for handling the requests from the frontend and running racket queries.

The backend serves the build files from the /build directory.

The backend uses socket.io to communicate with the frontend.
This will allow us to potentially add more features, such as continuously streaming
the output of the profiler.

The backend creates a folder called /tmp, and subfolders based on the socket connection id.
The user's code is saved as `user-code.rkt`. This will likely need to be changed for multi file support.

The backend uses the [child_process](https://nodejs.org/api/child_process.html) 
to spawn a docker container and run the user's code. The docker container provides isolation so that each users
request is run in a separate environment.

The backend is also run inside a docker container, for ease of deployment.