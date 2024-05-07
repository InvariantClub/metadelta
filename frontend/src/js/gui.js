import { GUI } from "lil-gui";

export function setupGui ( { onlyChangedUpdate, regexFilterUpdate } )  {
  const f = new GUI(
    { width: 350
    , title: "🔎 Filtering"
    }
  );
  f.hide();
  f.close();

  // TODO: Ideally these initial values would come from the UI.
  const obj =
    { "Table regex": ""
    , "Role regex": ""
    , "Column regex": ""
    , "Schema regex": ""
    , "Database regex": ""
    , "Only changed permissions": true
    }

  f.add( obj, "Only changed permissions" );
  f.add( obj, "Database regex" );
  f.add( obj, "Schema regex" );
  f.add( obj, "Table regex" );
  f.add( obj, "Role regex" );
  f.add( obj, "Column regex" );


  f.onChange( ({ property, value }) => {
    if (property === "Only changed permissions" ){
      onlyChangedUpdate.send(value);
    }

    if (property === "Table regex") {
      regexFilterUpdate.send({"field": "Table", "regex": value})
    }

    if (property === "Role regex") {
      regexFilterUpdate.send({"field": "Role", "regex": value})
    }

    if (property === "Column regex") {
      regexFilterUpdate.send({"field": "Column", "regex": value})
    }

    if (property === "Schema regex") {
      regexFilterUpdate.send({"field": "Schema", "regex": value})
    }

    if (property === "Database regex") {
      regexFilterUpdate.send({"field": "Database", "regex": value})
    }
  });

  return f;
}
