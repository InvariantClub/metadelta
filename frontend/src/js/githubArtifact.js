import * as zip    from "@zip.js/zip.js";
import { Octokit } from "octokit";

export async function downloadDiff ( { artifact_id, owner, repo, token })  {
  // TODO: Don't re-download if we've already got it in some cache.
  const octokit = new Octokit({ auth: token });
  const meta = await octokit.request('GET /repos/{owner}/{repo}/actions/artifacts/{artifact_id}/zip',
    { owner: owner
    , repo: repo
    , artifact_id: artifact_id
    , headers: { 'X-GitHub-Api-Version': '2022-11-28' }
    }
  );

  const rawZip    = await fetch(meta.url);
  const blob      = new zip.BlobReader(await rawZip.blob());
  const zipReader = new zip.ZipReader(blob);

  try {
    const writer     = new zip.TextWriter();
    const firstEntry = (await zipReader.getEntries()).shift();
    const diffText   = await firstEntry.getData(writer);
    return diffText
  } finally {
    await zipReader.close();
  }
}
