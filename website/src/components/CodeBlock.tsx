import { useState } from 'react';
import HighlightedCode, { type HighlightLanguage } from './HighlightedCode';

type CodeBlockProps = {
  code: string;
  language?: HighlightLanguage;
};

function CodeBlock({ code, language = 'forge' }: CodeBlockProps) {
  const [copied, setCopied] = useState(false);

  async function onCopy() {
    try {
      await navigator.clipboard.writeText(code);
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1500);
    } catch {
      setCopied(false);
    }
  }

  return (
    <div className="code-block">
      <button className="copy-btn" data-copied={copied ? '1' : '0'} onClick={onCopy}>
        {copied ? 'Copied' : 'Copy'}
      </button>
      <pre>
        <HighlightedCode code={code} language={language} />
      </pre>
    </div>
  );
}

export default CodeBlock;
