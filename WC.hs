main = interact wordCount
  where wordCount input = show (length input) ++ "\n"
  -- lines takes the file as a long string and breaks it into a list, using the new-line as the break point.
  -- length just counts the list
  -- Counting characters will include the new line breaks. Thus, substract by the number of lines if we are disregarding '\n'
  -- Both options are shown in the chapter-1-exercise file.
