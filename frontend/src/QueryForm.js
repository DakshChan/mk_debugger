import { useEffect, useState } from "react";
import {
  Input, Textarea,
  NumberInput, NumberInputField, NumberInputStepper, NumberIncrementStepper, NumberDecrementStepper,
  Tooltip
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
      queryVars: `(${queryVars})`, query}
    });
  }, [setQueryData, solutions, samples, steps, queryVars, query, id]);

  return (
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
        <Textarea size={"sm"} resize={"both"} placeholder={"Query"} value={query}
                  onChange={event => setQuery(event.target.value)} width={"unset"} height={"unset"}/>
      </Tooltip>
      <p style={{margin: "0", fontSize:"1.3em"}}>)</p>
    </div>
  );
}