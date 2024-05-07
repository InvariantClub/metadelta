import { downloadDiff } from "./js/githubArtifact.js";
import { setupGui } from "./js/gui.js";

export const flags = ({ env }) => {
  // TODO: Unify the key name here with the one in elm.
  return { localSettings: { githubToken: localStorage.getItem("githubToken") } }
}


export const onReady = ({ app, env }) => {
  if ( app.ports
       && app.ports.onlyChangedUpdate
       && app.ports.regexFilterUpdate
       && app.ports.hideFilterWindow
       && app.ports.showFilterWindow
    ) {
    const g = setupGui(
      { onlyChangedUpdate: app.ports.onlyChangedUpdate
      , regexFilterUpdate: app.ports.regexFilterUpdate
      } );
    app.ports.showFilterWindow.subscribe(() => {
      g.show();
    });
    app.ports.hideFilterWindow.subscribe(() => {
      g.hide();
    });
  }


  if ( app.ports && app.ports.downloadGithubArtifact ) {
    app.ports.downloadGithubArtifact.subscribe((artifactInfo) => {
      downloadDiff(artifactInfo).then( (json) => {
        if (json) {
          app.ports.receiveUnzippedJson.send(json);
        } else {
          app.ports.notifyError.send("Could not compute diff.");
        }
      }).catch( (err) => {
        app.ports.notifyError.send("JavaScript Error: " + err);
      });
    });
  }


  if ( app.ports && app.ports.writeLocalStorage ) {
    app.ports.writeLocalStorage.subscribe(({ key, value }) => {
      localStorage.setItem(key, value);
    });
  }


  if ( app.ports && app.ports.queryGithubToken ) {
    app.ports.queryGithubToken.subscribe(({ key }) => {
      const v = localStorage.getItem(key);
      if ( v ) {
        app.ports.readGithubToken.send(v);
      }
    });
  }
}
