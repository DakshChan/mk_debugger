import QueryForm from "./QueryForm";
import { useState } from "react";
import { v4 as uuidv4 } from 'uuid';
import { Button } from "@chakra-ui/react";
import { socket } from "./socket";

export default function QueryPanel({setQueries}) {
  const [forms, setForms] = useState([uuidv4()]);

  const removeForm = (id, index) => {
    setForms(forms.filter((e) => e !== id));
    setQueries({});
    socket.emit('kill', id, (data) => {
      console.log(data);
    });
  }

  return (
    <div className={"query-panel"} style={{display: "flex", flexDirection: "column", gap: "0.2em"}}>
      {
        forms.map((id, index) => {
          return <QueryForm key={id} id={id} index={index} removeForm={removeForm} setQueries={setQueries}/>;
        })
      }
      <div>
        <Button size={'sm'} onClick={() => {setForms([...forms, uuidv4()])}}>Add Query</Button>
      </div>
    </div>
  );
}