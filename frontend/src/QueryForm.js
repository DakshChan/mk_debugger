import { useEffect, useState } from "react";
import {
  Input, Textarea,
  NumberInput, NumberInputField, NumberInputStepper, NumberIncrementStepper, NumberDecrementStepper,
  Tooltip, useToast, CloseButton
} from "@chakra-ui/react";
import RunResume from "./RunResume";
import { socket } from "./socket";

export default function QueryForm({id, removeForm, index, queries, setQueries}) {
  const toast = useToast();

  const [solutions, setSolutions] = useState("");
  const [samples, setSamples] = useState("");
  const [steps, setSteps] = useState("");
  const [queryVars, setQueryVars] = useState("");
  const [query, setQuery] = useState("");

  const [queryData, setQueryData] = useState("");

  const [status, setStatus] = useState("");
  const [debug, setDebug] = useState(undefined);
  const statusColor = {running: "blue", success: "green", error: "red", "": "grey"};

  useEffect(() => {
    setQueryData({
      solutions: (solutions !== "") ? solutions : "#f",
      samples: (samples !== "") ? samples : "#f",
      steps: (steps !== "") ? steps : "#f",
      queryVars: `(${queryVars})`, query, id
    });
  }, [setQueryData, solutions, samples, steps, queryVars, query, id]);

  useEffect(() => {
    if (debug !== undefined) {
      setQueries(
        {...queries, [index]: debug}
      );
    }
  }, [debug, index, setQueries, debug]);

  const sendQuery = (q) => {
    setStatus("running");
    q = {...q, ...queryData};
    console.log(q);
    socket.emit('query', id, q, (data) => {
      console.log(data);
      setDebug(data.data);
      setStatus(data.status === 200 ? "success" : "error");
      toast({
        position: "bottom-left",
        title: data.message,
        status: (data.status === 200) ? "success" : "error",
        ...((data.status === 500) ? {description: data.body} : {}),
        isClosable: true
      });
    });
  }

  const sendKill = () => {
    socket.emit('kill', id, (data) => {
      console.log(data);
      setStatus("");
      toast({
        position: "bottom-left",
        title: "Racket process killed",
        status: "success",
        isClosable: true
      });
    });
  };

  return (
    <div style={{display: "flex", gap: "0.2em",
      borderRadius: "0.5em", borderWidth: "0.2em", borderColor: statusColor[status]}}>
      <p style={{margin: "0", fontSize:"1.5em", whiteSpace:"nowrap", alignSelf: "center"}}>{index}:</p>
      <p style={{margin: "0", fontSize:"1.3em", whiteSpace:"nowrap"}}>( run </p>
      <div style={{display: "flex", flexDirection: "column", gap:"0.1em"}}>
        <Tooltip label={'Solutions'} placement={'right'}>
          <NumberInput size={"sm"} step={1} precision={0} min={0}
                       value={solutions} onChange={event => setSolutions(event)}>
            <NumberInputField placeholder={"Solutions"}/>
            <NumberInputStepper>
              <NumberIncrementStepper />
              <NumberDecrementStepper />
            </NumberInputStepper>
          </NumberInput>
        </Tooltip>
        <Tooltip label={'Samples'} placement={'right'}>
          <NumberInput size={"sm"} step={1} precision={0} min={0}
                     value={samples} onChange={event => setSamples(event)}>
            <NumberInputField placeholder={"Samples"}/>
            <NumberInputStepper>
              <NumberIncrementStepper />
              <NumberDecrementStepper />
            </NumberInputStepper>
          </NumberInput>
        </Tooltip>
        <Tooltip label={'Steps'} placement={'right'}>
          <NumberInput size={"sm"} step={1} precision={0} min={0}
                     value={steps} onChange={event => setSteps(event)}>
            <NumberInputField placeholder={"Steps"}/>
            <NumberInputStepper>
              <NumberIncrementStepper />
              <NumberDecrementStepper />
            </NumberInputStepper>
          </NumberInput>
        </Tooltip>
        <div style={{display: "flex", gap: "0.2em", fontSize: "1.2em"}}>
          (
          <Tooltip label={'Query variables'} placement={'right'}>
            <Input size={"sm"} type={"text"} placeholder={"Query variables"} value={queryVars}
                 onChange={event => setQueryVars(event.target.value)}/>
          </Tooltip>
          )
        </div>
      </div>
      <Tooltip label={'Query'} placement={'right'}>
        <Textarea size={"sm"} resize={"both"} placeholder={"Query"} value={query}
                  onChange={event => setQuery(event.target.value)} width={"unset"} height={"unset"}/>
      </Tooltip>
      <p style={{margin: "0", fontSize:"1.3em"}}>)</p>
      <div style={{display: "flex", flexGrow: "1", justifyContent: "space-between"}}>
        <div>
          <RunResume sendQuery={sendQuery} status={status} debug={debug} sendKill={sendKill}/>
        </div>
        <CloseButton onClick={() => removeForm(id, index)}/>
      </div>
    </div>
  );
}