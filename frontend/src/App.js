import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import PointInfoPanel from "./PointInfoPanel";
import { useEffect, useState } from "react";
import { Select, useToast } from "@chakra-ui/react";
import { Alert, AlertIcon, AlertTitle } from '@chakra-ui/react'
import QueryPanel from "./QueryPanel";
import { socket } from "./socket";
import StateInfoPanel from "./StateInfoPanel";

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "failures", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");
  const [queries, setQueries] = useState({});
  const [serverConnected, setServerConnected] = useState(true);
  const [aggregation, setAggregation] = useState("sum");
  const [diff, setDiff] = useState("");

  const toast = useToast();

  useEffect(() => {
    socket.on('connect', () => {
      console.log("Connected to server");
      setServerConnected(true);
      toast({
        position: "bottom-left",
        title: "Connected to server",
        status: "success",
        duration: 3000,
        isClosable: true
      })
    });

    socket.on('connect_error', () => {
      console.log("Connection to server failed");
      setServerConnected(false);
      toast({
        position: "bottom-left",
        title: "Connection to server failed",
        status: "error",
        duration: 3000,
        isClosable: true
      })
    });

    socket.on('disconnect', () => {
      console.log("Disconnected from server");
      setServerConnected(false);
    });

    socket.on('exit', (id, code, signal) => {
      console.log(`Racket process ${id} exited with code ${code} and signal ${signal}`);
      toast({
        position: "bottom-left",
        title: "Racket process exited",
        description: `Code: ${code}, Signal: ${signal}`,
        status: "error",
        isClosable: true
      });
    });

    return () => {
      socket.off('connect');
      socket.off('connect_error');
      socket.off('disconnect');
      socket.off('exit');
    };
  }, [toast]);

  const sendCode = (file, fileName) => {
    socket.emit('code', file, fileName, (data) => {
      console.log(data);
      setCode((new TextDecoder("utf-8")).decode(data.file));
      setQueries({});
      setFileName(data.fileName);
      toast({
        position: "bottom-left",
        title: "Code uploaded",
        status: "success",
        isClosable: true
      });
    });
  };

  useEffect(() => {
    setPointDebug(undefined);
    if (Object.keys(queries).length === 0) {
      setDebug(undefined);
      return;
    }
    if (aggregation === "sum") {
      let pp = [];
      for (let q in Object.keys(queries)) {
        q = Object.keys(queries)[q];
        for (let p in queries[q]["program-points"]) {
          p = structuredClone(queries[q]["program-points"][p]);
          let e = pp.find((e) => {return e.syntax.span === p.syntax.span && e.syntax.position === p.syntax.position});
          if (e === undefined) {
            pp.push(p);
          } else {
            e.count += p.count;
            e.fails += p.fails;
            e.success += p.success;
          }
        }
      }
      setDebug({"program-points": pp});
      return;
    }
    if (aggregation === "min") {
      let pp = [];
      for (let q in Object.keys(queries)) {
        q = Object.keys(queries)[q];
        for (let p in queries[q]["program-points"]) {
          p = structuredClone(queries[q]["program-points"][p]);
          let e = pp.find((e) => {return e.syntax.span === p.syntax.span && e.syntax.position === p.syntax.position});
          if (e === undefined) {
            pp.push(p);
          } else {
            e.count = Math.min(e.count, p.count);
            e.fails = Math.min(e.count, p.fails);
            e.success = Math.min(e.count, p.success);
          }
        }
      }
      setDebug({"program-points": pp});
      return;
    }
    if (aggregation === "max") {
      let pp = [];
      for (let q in Object.keys(queries)) {
        q = Object.keys(queries)[q];
        for (let p in queries[q]["program-points"]) {
          p = structuredClone(queries[q]["program-points"][p]);
          let e = pp.find((e) => {return e.syntax.span === p.syntax.span && e.syntax.position === p.syntax.position});
          if (e === undefined) {
            pp.push(p);
          } else {
            e.count = Math.max(e.count, p.count);
            e.fails = Math.max(e.count, p.fails);
            e.success = Math.max(e.count, p.success);
          }
        }
      }
      setDebug({"program-points": pp});
      return;
    }
    if (aggregation === "average") {
      let pp = [];
      for (let q in Object.keys(queries)) {
        q = Object.keys(queries)[q];
        for (let p in queries[q]["program-points"]) {
          p = structuredClone(queries[q]["program-points"][p]);
          let e = pp.find((e) => {return e.syntax.span === p.syntax.span && e.syntax.position === p.syntax.position});
          if (e === undefined) {
            pp.push(p);
          } else {
            e.count += p.count;
            e.fails += p.fails;
            e.success += p.success;
          }
        }
      }
      for (let p in pp) {
        p = pp[p];
        p.count /= Object.keys(queries).length;
        p.fails /= Object.keys(queries).length;
        p.success /= Object.keys(queries).length;
      }
      setDebug({"program-points": pp});
      return;
    }
    if (aggregation === "minuend" && queries[diff] !== undefined) {
      let pp = structuredClone(queries[diff]["program-points"]);
      for (let q in Object.keys(queries)) {
        q = Object.keys(queries)[q];
        if (q === diff) {
          continue;
        }
        for (let p in queries[q]["program-points"]) {
          p = structuredClone(queries[q]["program-points"][p]);
          let e = pp.find((e) => {return e.syntax.span === p.syntax.span && e.syntax.position === p.syntax.position});
          if (e === undefined) {
            // pp.push(p);
          } else {
            e.count -= p.count;
            e.fails -= p.fails;
            e.success -= p.success;
          }
        }
      }
      setDebug({"program-points": pp});
      return;
    }
    if (aggregation === "single" && queries[diff] !== undefined) {
      let pp = structuredClone(queries[diff]["program-points"]);
      setDebug({"program-points": pp});
      return;
    }
  }, [queries, diff, aggregation, code]);

  useEffect(() => {
    if (Object.keys(queries).length === 0) {
      setDiff("");
    } else {
      setDiff(Object.keys(queries)[0]);
    }
  }, [queries, setDiff]);

  return (
    <div style={{display: "flex", flexDirection: "column", gap: "0.2em"}}>
      {(serverConnected) ? <></> :
        <Alert status='error'>
          <AlertIcon />
          <AlertTitle>Server not connected!</AlertTitle>
        </Alert>
      }
      <div style={{display: "flex", gap: "0.2em"}}>
        <UploadCode sendCode={sendCode} fileName={fileName}/>
      </div>
      <div style={{display: "flex", gap: "0.2em"}}>
        <div style={{display: "flex", flexDirection: "column", gap:"0.2em"}}>
          <QueryPanel setQueries={setQueries} queries={queries}/>
          <div style={{display: "flex", alignItems:"baseline", gap: "1ch"}}>
            <p>Highlighting</p>
            <Select width={"unset"} size={"sm"} defaultValue={"failures"} onChange={(event) => setCodeHighlight({...codeHighlight, "info": event.target.value})}>
              <option value={"encounters"}>Encounters</option>
              <option value={"failures"}>Failures</option>
              <option value={"failRatio"}>Failure Ratio</option>
              <option value={"successes"}>Successes</option>
              <option value={"successRatio"}>Success Ratio</option>
            </Select>
            <p>Aggregation</p>
            <Select width={"unset"} size={"sm"} defaultValue={"sum"} onChange={(event) => setAggregation(event.target.value)}>
              <option value={"sum"}>Sum</option>
              <option value={"minuend"}>Minuend</option>
              <option value={"average"}>Average</option>
              <option value={"min"}>Min</option>
              <option value={"max"}>Max</option>
              <option value={"single"}>Single</option>
            </Select>
            {
              (aggregation === "minuend" || aggregation === "single") ?
                <>
                  <p>Query</p>
                  <Select width={"unset"} size={"sm"} defaultValue={"0"} onChange={(event) => setDiff(event.target.value)}>
                    {
                      Object.keys(queries).map((q) => {
                        return <option key={q} value={q}>{(parseInt(q)+1)}</option>
                      })
                    }
                  </Select>
                </> :
                <></>
            }
          </div>
          <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} pointDebug={pointDebug} setPointDebug={setPointDebug}/>
        </div>
        <div style={{width: "-webkit-fill-available", padding:"1em",
          display:"flex", flexDirection:"column", gap:"1em"}}>
          <PointInfoPanel queries={queries} pointDebug={pointDebug} code={code}/>
          <StateInfoPanel queries={queries} code={code}/>
        </div>
      </div>
    </div>
  );
}

export default App;
