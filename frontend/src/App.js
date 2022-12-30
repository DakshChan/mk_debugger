import CodeContainer from "./CodeContainer";
import UploadCode from "./UploadCode";
import PointInfoPanel from "./PointInfoPanel";
import { useEffect, useState } from "react";
import './App.css';
import SolutionInfoPanel from "./SolutionInfoPanel";
import RejectionInfoPanel from "./RejectionInfoPanel";
import RunResume from "./RunResume";
import { Select, useToast } from "@chakra-ui/react";
import {
  Alert,
  AlertIcon,
  AlertTitle,
} from '@chakra-ui/react'
import QueryPanel from "./QueryPanel";
import { socket } from "./socket";

function App() {
  const [code, setCode] = useState("");
  const [debug, setDebug] = useState(undefined);
  const [codeHighlight, setCodeHighlight] = useState({"info": "failures", "style": "color"});
  const [pointDebug, setPointDebug] = useState(undefined);
  const [fileName, setFileName] = useState("");
  const [running, setRunning] = useState(false);
  const [queries, setQueries] = useState({});
  const [serverConnected, setServerConnected] = useState(true);
  const [aggregation, setAggregation] = useState("union");
  const [diff, setDiff] = useState("0");

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
      setRunning(false);
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
      setRunning(false);
      toast({
        position: "bottom-left",
        title: "Code uploaded",
        status: "success",
        isClosable: true
      });
    });
  };

  useEffect(() => {
    if (queries.length === 0) {
      setDebug(undefined);
      return;
    }
    if (aggregation === "union") {
      let pp = [];
      for (const q in Object.keys(queries)) {
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
    if (aggregation === "difference") {
      let pp = structuredClone(queries[diff]["program-points"]);
      for (const q in Object.keys(queries)) {
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
    if (aggregation === "single") {
      let pp = structuredClone(queries[diff]["program-points"]);
      setDebug({"program-points": pp});
      return;
    }
  }, [queries, diff, aggregation]);

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
            <Select width={"unset"} size={"sm"} defaultValue={"union"} onChange={(event) => setAggregation(event.target.value)}>
              <option value={"union"}>Union</option>
              <option value={"difference"}>Difference</option>
              <option value={"single"}>Single</option>
            </Select>
            {
              (aggregation === "difference" || aggregation === "single") ?
                <>
                  <p>Aggregation Num</p>
                  <Select width={"unset"} size={"sm"} defaultValue={"0"} onChange={(event) => setDiff(event.target.value)}>
                    {
                      Object.keys(queries).map((q) => {
                        return <option value={q}>{q}</option>
                      })
                    }
                  </Select>
                </> :
                <></>
            }
          </div>
          <CodeContainer code={code} debug={debug} codeHighlight={codeHighlight} pointDebug={pointDebug} setPointDebug={setPointDebug}/>
        </div>
        <div style={{width: "-webkit-fill-available"}}>
          <PointInfoPanel pointDebug={pointDebug} code={code}/>
          <SolutionInfoPanel queries={queries} code={code}/>
          <RejectionInfoPanel queries={queries} code={code}/>
        </div>
      </div>
    </div>
  );
}

export default App;
