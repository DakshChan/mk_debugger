import { useEffect, useState } from "react";
import {
  Input, Textarea, Tooltip, CloseButton, Heading, Button, useDisclosure, useToast,
  NumberInput, NumberInputField, NumberInputStepper, NumberIncrementStepper, NumberDecrementStepper,
  Modal, ModalOverlay, ModalContent, ModalHeader, ModalCloseButton, ModalBody, Checkbox,
} from "@chakra-ui/react";
import RunResume from "./RunResume";
import { socket } from "./socket";

export default function QueryForm({id, removeForm, index, setQueries}) {
  const toast = useToast();

  const [solutions, setSolutions] = useState("");
  const [samples, setSamples] = useState("");
  const [steps, setSteps] = useState("");
  const [queryVars, setQueryVars] = useState("");
  const [query, setQuery] = useState("");

  const [queryData, setQueryData] = useState("");

  const [status, setStatus] = useState("not run");
  const [debug, setDebug] = useState(undefined);
  const statusColor = {running: "blue", success: "green", error: "red", "not run": "grey"};
  const [error, setError] = useState(undefined);
  const { isOpen, onOpen, onClose } = useDisclosure();
  const [include, setInclude] = useState(true);

  useEffect(() => {
    setQueryData({
      solutions: (solutions !== "") ? solutions : "#f",
      samples: (samples !== "") ? samples : "#f",
      steps: (steps !== "") ? steps : "#f",
      queryVars: `(${queryVars})`, query, id
    });
  }, [setQueryData, solutions, samples, steps, queryVars, query, id]);

  useEffect(() => {
    if (debug !== undefined && include) {
      setQueries(queries => ({...queries, [index]: debug}));
      return
    }
    if (include === false) {
      setQueries(queries => {let q = structuredClone(queries); delete q[index]; return q});
    }
  }, [debug, index, setQueries, include]);

  const sendQuery = (q) => {
    setStatus("running");
    q = {...q, ...queryData};
    console.log(q);
    socket.emit('query', id, q, (data) => {
      console.log(data);
      setDebug(data.data);
      setStatus(data.status === 200 ? "success" : "error");
      if (data.status !== 200) {
        setError(data);
      }
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
    <div style={{display: "flex", flexDirection: "column", gap: "0.2em", padding: "0.1em",
      borderRadius: "0.6em", borderWidth: "0.2em", borderColor: statusColor[status]}}>
      <div style={{display: "flex", gap: "0.2em", alignItems: "center"}}>
        <Checkbox isChecked={include} size={'lg'} onChange={(e) => setInclude(e.target.checked)}></Checkbox>
        <Heading size={'md'}>Query {index + 1}</Heading>
        <RunResume sendQuery={sendQuery} status={status} debug={debug} sendKill={sendKill}/>
        {
          (status === "error") ?
            <>
              <Button size={"sm"} onClick={onOpen}>View Error</Button>

              <Modal isOpen={isOpen} onClose={onClose}>
                <ModalOverlay />
                <ModalContent>
                  <ModalHeader>{error.message}</ModalHeader>
                  <ModalCloseButton />
                  {
                    error.status === 500 ?
                      <ModalBody>
                        {error.body}
                      </ModalBody>
                      : <></>
                  }
                  </ModalContent>
              </Modal>
          </> :
          <></>
        }
        <div style={{flexGrow: 1, display: "flex", justifyContent: "flex-end", alignItems: "center"}}>
          <p>Status: {status}</p>
          <CloseButton onClick={() => removeForm(id, index)}/>
        </div>
      </div>
      <div style={{display: "flex", gap: "0.2em"}}>
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
          <Textarea size={"sm"} resize={"both"} placeholder={"Query"} value={query} style={{flexGrow: 1}}
                    onChange={event => setQuery(event.target.value)} width={"unset"} height={"unset"}/>
        </Tooltip>
        <p style={{margin: "0", fontSize:"1.3em"}}>)</p>
      </div>
    </div>
  );
}