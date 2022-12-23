import { useEffect, useState } from "react";
import {
  Input, Textarea,
  NumberInput, NumberInputField, NumberInputStepper, NumberIncrementStepper, NumberDecrementStepper
} from "@chakra-ui/react";

export default function QueryForm({id, queryData, setQueryData}) {
  const [solutions, setSolutions] = useState("");
  const [samples, setSamples] = useState("");
  const [steps, setSteps] = useState("");
  const [queryVars, setQueryVars] = useState("");
  const [query, setQuery] = useState("");

  useEffect(() => {
    setQueryData({...queryData, [id]: {
      solutions: (solutions !== "") ? solutions : "#f",
      samples: (samples !== "") ? samples : "#f",
      steps: (steps !== "") ? steps : "#f",
        queryVars, query}
    });
  }, [setQueryData, solutions, samples, steps, queryVars, query, id]);

  return (
    <div style={{display: "flex", gap: "0.2em"}}>
      <p style={{margin: "0", fontSize:"1.3em"}}>( run </p>
      <div style={{display: "flex", flexDirection: "column", gap:"0.1em"}}>
        <NumberInput size={"sm"} step={1} precision={0} min={0}
                     value={solutions} onChange={event => setSolutions(event)}>
          <NumberInputField placeholder={"solutions"}/>
          <NumberInputStepper>
            <NumberIncrementStepper />
            <NumberDecrementStepper />
          </NumberInputStepper>
        </NumberInput>
        <NumberInput size={"sm"} step={1} precision={0} min={0}
                     value={samples} onChange={event => setSamples(event)}>
          <NumberInputField placeholder={"samples"}/>
          <NumberInputStepper>
            <NumberIncrementStepper />
            <NumberDecrementStepper />
          </NumberInputStepper>
        </NumberInput>
        <NumberInput size={"sm"} step={1} precision={0} min={0}
                     value={steps} onChange={event => setSteps(event)}>
          <NumberInputField placeholder={"steps"}/>
          <NumberInputStepper>
            <NumberIncrementStepper />
            <NumberDecrementStepper />
          </NumberInputStepper>
        </NumberInput>
        <Input size={"sm"} type={"text"} placeholder={"query variables"} value={queryVars}
               onChange={event => setQueryVars(event.target.value)}/>
      </div>
      <Textarea size={"sm"} resize={"both"} placeholder={"query"} value={query}
                onChange={event => setQuery(event.target.value)} width={"unset"} height={"unset"}/>
      <p style={{margin: "0", fontSize:"1.3em"}}>)</p>
    </div>
  );
}