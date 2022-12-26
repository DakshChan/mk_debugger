import "./InfoPanel.css"
import {useEffect, useState} from "react";
import {CloseButton, Collapse, Heading, useDisclosure} from "@chakra-ui/react";

export default function PointInfoPanel({pointDebug, code}) {
  const [state, setState] = useState(undefined);
  const { isOpen, onOpen, onClose } = useDisclosure()

  useEffect(() => {
    if (pointDebug !== undefined) {
      setState(pointDebug);
      onOpen();
    } else {
      setState(undefined);
      onClose();
    }
  }, [pointDebug]);

  useEffect(() => {
    setState(undefined);
    onClose();
  }, [code]);

  return (
    <div className={"point-info-panel"}>
      <div style={{display: "flex"}}>
        <Heading size={"md"} style={{flexGrow: "1"}}>Point Panel</Heading>
        <CloseButton onClick={onClose}/>
      </div>
      <Collapse in={isOpen} animateOpacity>
        { state === undefined ? <></> :
        <>
          <div style={{display: "flex", gap: "3ch"}}>
           <p style={{whiteSpace: "nowrap"}}>Encounters: {state.count}</p>
           <p style={{whiteSpace: "nowrap"}}>Failures: {state.fails}</p>
           <p style={{whiteSpace: "nowrap"}}>Successes: {state.successes}</p>
          </div>
          <div style={{display: "flex", gap: "3ch"}}>
           <p style={{whiteSpace: "nowrap"}}>Line: {state.syntax.line}</p>
           <p style={{whiteSpace: "nowrap"}}>Column: {state.syntax.column}</p>
          </div>
          <div>
             <p style={{whiteSpace: "break-spaces"}}>Syntax: {state.syntax.content}</p>
          </div>
        </>
        }
      </Collapse>
    </div>
  );
}
