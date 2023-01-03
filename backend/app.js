const express = require('express');
const path = require('path');
const cors = require('cors');
const { spawn } = require('child_process');
const fs = require('fs');
const socket = require("socket.io");

const app = express();
const port = process.env.PORT || 3000;

app.use(express.static(path.join(__dirname, 'build')));

// cors only being used when running react server, remove after react build
app.use(cors({
  origin: '*'
}));

app.get('(/*)?', (req, res) => {
  res.sendFile(path.join(__dirname, 'build', 'index.html'));
});

const server = app.listen(port, () => {
  if (!fs.existsSync("./tmp")){
    fs.mkdirSync("./tmp");
  }
  console.log(`Example app listening on port ${port}`)
  console.log(`http://localhost:${port}`);
})

const io = socket(server, {cookie: false, maxHttpBufferSize: 1e8, cors: {origin: "*"}});

io.on("connection", (socket) => {
  { // initialization
    console.log("made socket connection", socket.id);
    socket.mk_profiler = {queries: {}, codeContent: null}; // queries: {id: {racket: null, racketBuf: null}
    fs.mkdirSync(`./tmp/${socket.id}`);
  }

  socket.on("disconnect", () => {
    console.log("socket disconnected", socket.id);
    for (let id in Object.keys(socket.mk_profiler.queries)) {
      socket.mk_profiler.queries[id]?.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.kill();
      delete socket.mk_profiler.queries[id];
    }
    delete socket.mk_profiler;
    fs.rmSync(`./tmp/${socket.id}`, {recursive: true});
  });

  socket.on("kill", (id, callback) => {
    if (Object.keys(socket.mk_profiler.queries).includes(id)) {
      socket.mk_profiler.queries[id]?.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.kill();
      delete socket.mk_profiler.queries[id];
      callback({ status: 200, message: "killed" });
    } else {
      callback({ status: 200, message: "not running" });
    }
  });

  socket.on("code", (file, fileName, callback) => {
    for (let id in Object.keys(socket.mk_profiler.queries)) {
      socket.mk_profiler.queries[id]?.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.kill();
      delete socket.mk_profiler.queries[id];
    }
    fs.writeFileSync(`./tmp/${socket.id}/user-code.rkt`, file);
    socket.mk_profiler.codeContent = file;
    callback({ status: 200, message: "received", fileName: fileName, file: file });
  });

  socket.on("query", (id, query, callback) => {
    if (socket.mk_profiler.codeContent === null) {
      callback({ status: 400, message: "No code uploaded" });
      return;
    }
    console.log("query", query);

    if (!(query.command === "resume" || query.command === "run")) {
      callback({ status: 400, message: "Invalid command" });
      return;
    }

    if (query.command === "resume") {
      if (!(Object.keys(socket.mk_profiler.queries).includes(id))) {
        callback({ status: 400, message: "Process not running" });
        return;
      }
    }

    if (!(Object.keys(socket.mk_profiler.queries).includes(id))) {
      socket.mk_profiler.queries[id] = {racket: null, racketBuf: ""};
    }

    socket.mk_profiler.queries[id].racketBuf = "";
    socket.mk_profiler.queries[id]?.racket?.stdout?.removeAllListeners();
    socket.mk_profiler.queries[id]?.racket?.stderr?.removeAllListeners();
    socket.mk_profiler.queries[id]?.racket?.removeAllListeners();

    if (query.command === "run") {
      socket.mk_profiler.queries[id].racket = spawn("bash", ["-c",
        `exec docker run --rm -i $(docker build --rm -q --build-arg userRepl=${socket.id} -f racketDockerfile .)`]);
    }

    console.log(JSON.stringify(query) + "\n");
    socket.mk_profiler.queries[id].racket.stdin.write(JSON.stringify(query) + "\n");

    socket.mk_profiler.queries[id].racket.stdout.on('data', (data) => {
      // console.log(`$stdout: ${data}`);
      socket.mk_profiler.queries[id].racketBuf += data;
      try {
        let resultJSON = JSON.parse(socket.mk_profiler.queries[id].racketBuf);
        // console.log(JSON.stringify(resultJSON));
        resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
        // console.log(JSON.stringify(resultJSON));

        socket.mk_profiler.queries[id].racket?.stdout?.removeAllListeners();
        socket.mk_profiler.queries[id].racket?.stderr?.removeAllListeners();
        socket.mk_profiler.queries[id].racket?.removeAllListeners();
        callback({ status: 200, message: "Query success", data: resultJSON });
      } catch (e) {
        // ignore because incomplete json, keep buffering
      }
    });

    socket.mk_profiler.queries[id].racket.stderr.on('data', (data) => {
      console.error(`stderr: ${data.toString()}`);

      socket.mk_profiler.queries[id].racketBuf = "";
      socket.mk_profiler.queries[id]?.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.queries[id]?.racket?.removeAllListeners(); // so that the exit event doesn't fire when we kill it
      socket.mk_profiler.queries[id]?.racket?.kill();
      socket.mk_profiler.queries[id].racket = null;

      callback({ status: 500, message: "Query error", body: data.toString() });
    });

    socket.mk_profiler.queries[id].racket.on('exit', (code, signal) => {
      socket.emit("exit", {id, code, signal});
    })
  });
});