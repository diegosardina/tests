MODULE Texts;

  IMPORT Out, Platform;


  CONST
    blockSize = 64;


  TYPE
    Text* = POINTER TO TLayout;

    Block = POINTER TO BlockLayout;

    TLayout = RECORD
      length : LONGINT;
      head, last, tail : Block;
    END;

    BlockLayout = RECORD
      index : LONGINT;
      part : ARRAY blockSize OF CHAR;
      past, next : Block;
    END;


  VAR
    test   : Text;
    outstr : ARRAY 10 OF CHAR;
    (* for benchmark *)
    text   : POINTER TO ARRAY OF CHAR;
    time1  : LONGINT;
    time2  : LONGINT;
    i, t   : LONGINT;


  PROCEDURE NewBlock (text : ARRAY OF CHAR; start : LONGINT; past : Block) : Block;
    VAR
      b : Block;
      i : INTEGER;
  BEGIN
    NEW(b);
    b.past := past;
    IF past # NIL THEN past.next := b END;
    (* Copying subarray *)
    i := 0;
    WHILE (i < blockSize) & (start+i < LEN(text)) DO
      b.part[i] := text[start+i];
      INC(i)
    END;
    b.index := start DIV blockSize;
    RETURN b
  END NewBlock;


  PROCEDURE NewText (length : LONGINT; head, tail : Block) : Text;
    VAR t : Text;
  BEGIN
    NEW(t);
    t.length := length;
    t.head   := head;
    t.tail   := tail;
    t.last   := head;
    RETURN t;
  END NewText;


  PROCEDURE Create* (text : ARRAY OF CHAR) : Text;
    VAR
      block, tmp : Block;
      remaining  : LONGINT;
  BEGIN
    remaining := LEN(text);
    tmp := NIL;
    REPEAT
      block := NewBlock(text, LEN(text) - remaining, tmp);
      tmp := block;
      DEC(remaining, blockSize);
    UNTIL remaining < 1;

    WHILE block.past # NIL DO block := block.past END;

    (* blocks := LEN(text) DIV blockSize - (-LEN(text) MOD blockSize) DIV LEN(text); *)

    RETURN NewText(LEN(text)-1, block, tmp);
  END Create;


  PROCEDURE GetBlockAt (t : Text; pos : LONGINT);
    VAR i : LONGINT;
  BEGIN
    i := t.last.index;
    (* If supported use WHILE - ELSIF *)
    LOOP IF i < pos DIV blockSize THEN
      t.last := t.last.next;
      INC(i)
    ELSIF i > pos DIV blockSize THEN
      t.last := t.last.past;
      DEC(i)
    ELSE EXIT END END
  END GetBlockAt;


  PROCEDURE ReadAt* (t : Text; index : LONGINT) : CHAR;
  BEGIN
    ASSERT(index <  t.length);
    GetBlockAt(t, index);
    RETURN t.last.part[index MOD blockSize]
  END ReadAt;


  PROCEDURE Search* (t : Text; pattern : ARRAY OF CHAR) : LONGINT;
    VAR
      i, j, k : LONGINT;
      d : ARRAY 128 OF LONGINT;
  BEGIN
    FOR i := 0 TO 127 DO d[i] := LEN(pattern)-1 END;
    FOR j := 0 TO LEN(pattern)-3 DO d[ORD(pattern[j])] := LEN(pattern)-j-2 END;
    i := LEN(pattern)-1;
    REPEAT
      j := LEN(pattern)-1;
      k := i;
      REPEAT
        DEC(k);
        DEC(j)
      UNTIL (j <= 0) OR (ReadAt(t, k) # pattern[j]);
      i := i + d[ORD(ReadAt(t, i-1))]
    UNTIL (j <= 0) OR (i > t.length);
    IF j <= 0 THEN RETURN k ELSE RETURN -1 END;
  END Search;


  PROCEDURE Search2* (text : ARRAY OF CHAR; pattern : ARRAY OF CHAR) : LONGINT;
    VAR
      i, j, k : LONGINT;
      d: ARRAY 128 OF LONGINT;
  BEGIN
    FOR i := 0 TO 127 DO d[i] := LEN(pattern)-1 END;
    FOR j := 0 TO LEN(pattern)-3 DO d[ORD(pattern[j])] := LEN(pattern)-j-2 END;
    i := LEN(pattern)-1;
    REPEAT
      j := LEN(pattern)-1;
      k := i;
      REPEAT
        DEC(k);
        DEC(j)
      UNTIL (j <= 0) OR (text[k] # pattern[j]);
      i := i + d[ORD(text[i-1])]
    UNTIL (j <= 0) OR (i > LEN(text)-1);
    IF j <= 0 THEN RETURN k ELSE RETURN -1 END;
  END Search2;


  PROCEDURE Read* (t : Text; start, offset : LONGINT; VAR out : ARRAY OF CHAR);
    VAR i, p : LONGINT;
  BEGIN
    GetBlockAt(t, start);
    i := 0;
    p := start MOD blockSize;
    (* If supported use WHILE - ELSIF *)
    LOOP IF (p < LEN(t.last.part)) & (i < offset) THEN
      out[i] := t.last.part[p];
      INC(p);
      INC(i)
    ELSIF i < offset THEN
      p := 0;
      t.last := t.last.next
    ELSE EXIT END END;
  END Read;


BEGIN
  NEW(text, 241);
  COPY("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc ultrices dictum lacus, vulputate porta lorem egestas vitae. Aenean dictum dui ex, eget pharetra ex viverra a. Praesent urna enim, tincidunt at consequat quis, faucibus ut ipsum.", text^);

  test := Create ("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc ultrices dictum lacus, vulputate porta lorem egestas vitae. Aenean dictum dui ex, eget pharetra ex viverra a. Praesent urna enim, tincidunt at consequat quis, faucibus ut ipsum.");
  Out.Ln;


  Out.Int(Search(test, "faucibus"), 0);
  Out.Ln;


  time1 := Platform.Time();
  FOR i := 0 TO 300000 - 1 DO
    t := Search(test, "lorem egestas");
  END;
  time2 := Platform.Time();
  Out.String("Search1 in ");
  Out.Int(time2-time1, 0);
  Out.String(" ms for ");
  Out.Int(i, 0);
  Out.Ln; Out.Ln;

  time1 := Platform.Time();
  FOR i := 0 TO 300000 - 1 DO
    t := Search2(text^, "lorem egestas");
  END;
  time2 := Platform.Time();

  Out.String("Search2 in ");
  Out.Int(time2-time1, 0);
  Out.String(" ms for ");
  Out.Int(i, 0);
  Out.Ln; Out.Ln;

END Texts.
