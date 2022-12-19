FROM docker:dind
RUN apk update
RUN apk add bash
RUN apk add --update nodejs npm

WORKDIR /profiler/backend
COPY ./backend/package.json ./
RUN npm install

WORKDIR /profiler/frontend
COPY ./frontend/package.json ./
RUN npm install
COPY ./frontend .
RUN npm run build

WORKDIR /profiler/backend
COPY ./backend .
RUN cp -r ../frontend/build .

CMD /bin/bash docker_wrapper.sh