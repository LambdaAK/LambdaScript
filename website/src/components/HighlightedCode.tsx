import { useMemo } from 'react';
import { highlightForge } from '../lib/forgeHighlight';

export type HighlightLanguage = 'forge' | 'plain';

type HighlightedCodeProps = {
  code: string;
  language?: HighlightLanguage;
  className?: string;
};

function HighlightedCode({ code, language = 'forge', className }: HighlightedCodeProps) {
  const tokens = useMemo(() => {
    if (language === 'plain') {
      return [{ text: code, kind: null }];
    }
    return highlightForge(code);
  }, [code, language]);

  return (
    <code className={className}>
      {tokens.map((token, index) => (
        <span
          key={`${index}-${token.text.length}`}
          className={token.kind ? `code-token token-${token.kind}` : undefined}
        >
          {token.text}
        </span>
      ))}
    </code>
  );
}

export default HighlightedCode;
