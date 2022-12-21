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

const io = socket(server, {cookie: false, maxHttpBufferSize: 1e8}); // 100mb buffer

io.on("connection", (socket) => {
  { // initialization
    console.log("made socket connection", socket.id);
    socket.mk_profiler = {running: false, racket: null, racketBuf: null, codeContent: null};
    fs.mkdirSync(`./tmp/${socket.id}`);
  }

  socket.on("disconnect", () => {
    console.log("socket disconnected", socket.id);
    if (socket.mk_profiler.running) {
      socket.mk_profiler.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.racket?.removeAllListeners();
      socket.mk_profiler.racket?.kill();
      socket.mk_profiler.racket = null;
      socket.mk_profiler.racketBuf = null;
      socket.mk_profiler.running = false;
    }
    fs.rmSync(`./tmp/${socket.id}`, {recursive: true});
  });

  socket.on("kill" , (callback) => {
    if (socket.mk_profiler.running) {
      socket.mk_profiler.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.racket?.removeAllListeners();
      socket.mk_profiler.racket.kill();
      socket.mk_profiler.racket = null;
      socket.mk_profiler.racketBuf = null;
      socket.mk_profiler.running = false;
      callback({ status: 200, message: "killed" });
    } else {
      callback({ status: 200, message: "not running" });
    }
  });

  socket.on("code", (file, fileName, callback) => {
    if (socket.mk_profiler.running) {
      socket.mk_profiler.racket.kill();
      socket.mk_profiler.racket = null;
      socket.mk_profiler.racketBuf = null;
      socket.mk_profiler.running = false;
    }
    fs.writeFileSync(`./tmp/${socket.id}/user-code.rkt`, file);
    socket.mk_profiler.codeContent = file;
    callback({ status: 200, message: "received", fileName: fileName, file: file });
  });

  socket.on("query", (query, callback) => {
    if (socket.mk_profiler.codeContent === null) {
      callback({ status: 400, message: "No code uploaded" });
      return;
    }
    console.log("query", query);

    socket.mk_profiler.racketBuf = "";
    socket.mk_profiler.racket?.stdout?.removeAllListeners();
    socket.mk_profiler.racket?.stderr?.removeAllListeners();
    socket.mk_profiler.racket?.removeAllListeners();

    if (query.command === "run") {
      socket.mk_profiler.racket = spawn("bash", ["-c", `exec docker run --rm -i $(docker build --rm -q --build-arg userRepl=${socket.id} -f racketDockerfile .)`]);
      socket.mk_profiler.running = true;
    } else if (query.command === "resume") {
      if (!socket.mk_profiler.running) {
        callback({ status: 400, message: "Process not running" });
        return;
      }
    } else {
      callback({ status: 400, message: "Invalid command" });
    }

    console.log(JSON.stringify(query) + "\n");
    socket.mk_profiler.racket.stdin.write(JSON.stringify(query) + "\n");

    socket.mk_profiler.racket.stdout.on('data', (data) => {
      // console.log(`$stdout: ${data}`);
      socket.mk_profiler.racketBuf += data;
      try {
        const resultJSON = JSON.parse(socket.mk_profiler.racketBuf);
        // console.log(JSON.stringify(resultJSON));
        resultJSON["program-points"] = resultJSON["program-points"].filter(x => x.syntax.source !== false)
        // console.log(JSON.stringify(resultJSON));

        socket.mk_profiler.racket?.stdout?.removeAllListeners();
        socket.mk_profiler.racket?.stderr?.removeAllListeners();
        socket.mk_profiler.racket?.removeAllListeners();
        callback({ status: 200, message: "Query success", data: resultJSON });
      } catch (e) {
        // ignore because incomplete json, keep buffering
      }
    });

    socket.mk_profiler.racket.stderr.on('data', (data) => {
      console.error(`stderr: ${data.toString()}`);

      socket.mk_profiler.racketBuf = "";
      socket.mk_profiler.running = false;
      socket.mk_profiler.racket?.stdout?.removeAllListeners();
      socket.mk_profiler.racket?.stderr?.removeAllListeners();
      socket.mk_profiler.racket?.removeAllListeners(); // so that the exit event doesn't fire when we kill it
      socket.mk_profiler.racket?.kill();
      socket.mk_profiler.racket = null;

      callback({ status: 500, message: "Query error", body: data.toString() });
    });

    socket.mk_profiler.racket.on('exit', (code, signal) => {
      socket.emit("exit", {code, signal});
    })
  });
});