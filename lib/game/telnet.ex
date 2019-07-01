defmodule Game.Telnet do
  def control_codes,
    do: %{
      # NULL. No operation
      "NUL" => 0,
      # Line Feed. Moves the printer to the next print line, keeping the same horizontal position.
      "LF" => 10,
      # Carriage Return. Moves the printer to the left margin of the current line.
      "CR" => 13,
      # BELL. Produces an audible or visible signal (which does NOT move the print head.
      "BEL" => 7,
      # Back Space. Moves the print head one character position towards the left margin. [On a printing devices this mechanism was commonly used to form composite characters by printing two basic characters on top of each other.]
      "BS" => 8,
      # Horizontal Tab. Moves the printer to the next horizontal tab stop. It remains unspecified how either party determines or establishes where such tab stops are located.
      "HT" => 9,
      # Vertical Tab. Moves the printer to the next vertical tab stop. It remains unspecified how either party determines or establishes where such tab stops are located.
      "VT" => 11,
      # Form Feed. Moves the printer to the top of the next page, keeping the same horizontal position. [On visual displays this commonly clears the screen and moves the cursor to the top left corner.]
      "FF" => 12
    }

  def commands,
    do: %{
      # End of subnegotiation parameters.
      "SE" => 240,
      # No operation
      "NOP" => 241,
      # Data mark. Indicates the position of a Synch event within the data stream. This should always be accompanied by a TCP urgent notification.
      "DM" => 242,
      # Break. Indicates that the "break" or "attention" key was hit.
      "BRK" => 243,
      # Suspend, interrupt or abort the process to which the NVT is connected.
      "IP" => 244,
      # Abort output. Allows the current process to run to completion but do not send its output to the user.
      "AO" => 245,
      # Are you there. Send back to the NVT some visible evidence that the AYT was received.
      "AYT" => 246,
      # Erase character. The receiver should delete the last preceding undeleted character from the data stream.
      "EC" => 247,
      # Erase line. Delete characters from the data stream back to but not including the previous CRLF.
      "EL" => 248,
      # Go ahead. Used, under certain circumstances, to tell the other end that it can transmit.
      "GA" => 249,
      # Subnegotiation of the indicated option follows.
      "SB" => 250,
      # Indicates the desire to begin performing, or confirmation that you are now performing, the indicated option.
      "WILL" => 251,
      # Indicates the refusal to perform, or continue performing, the indicated option.
      "WONT" => 252,
      # Indicates the request that the other party perform, or confirmation that you are expecting the other party to perform, the indicated option.
      "DO" => 253,
      # Indicates the demand that the other party stop performing, or confirmation that you are no longer expecting the other party to perform, the indicated option.
      "DONT" => 254,
      # Interpret as command
      "IAC" => 255
    }

  def reverse_commands,
    do:
      commands()
      |> Enum.map(fn {key, value} -> {value, key} end)
      |> Map.new()

  def options,
    do: %{
      "Transmit Binary" => 0,
      "Echo" => 1,
      "Reconnection" => 2,
      "Suppress Go Ahead" => 3,
      "Approx Message Size Negotiation." => 4,
      "Status" => 5,
      "Timing Mark" => 6,
      "Remote Controlled Trans and Echo" => 7,
      "Output Line Width" => 8,
      "Output Page Size" => 9,
      "Negotiate About Output Carriage-Return Disposition" => 10,
      "Negotiate About Output Horizontal Tabstops" => 11,
      "NAOHTD, Negotiate About Output Horizontal Tab Disposition" => 12,
      "Negotiate About Output Formfeed Disposition" => 13,
      "Negotiate About Vertical Tabstops" => 14,
      "Negotiate About Output Vertcial Tab Disposition" => 15,
      "Negotiate About Output Linefeed Disposition" => 16,
      "Extended ASCII." => 17,
      "Logout." => 18,
      "Byte Macro" => 19,
      "Data Entry Terminal" => 20,
      "SUPDUP" => 21,
      "SUPDUP Output" => 22,
      "Send Location" => 23,
      "Terminal Type" => 24,
      "End of Record" => 25,
      "TACACS User Identification" => 26,
      "Output Marking" => 27,
      "TTYLOC, Terminal Location Number." => 28,
      "Telnet 3270 Regime" => 29,
      "X.3 PAD." => 30,
      "NAWS, Negotiate About Window Size." => 31,
      "Terminal Speed" => 32,
      "Remote Flow Control" => 33,
      "Linemode" => 34,
      "X Display Location." => 35,
      "Environment" => 36,
      "Authentication" => 37,
      "Encryption Option" => 38,
      "New Environment" => 39,
      "TN3270E" => 40,
      "XAUTH" => 41,
      "CHARSET" => 42,
      "RSP, Telnet Remote Serial Port" => 43,
      "Com Port Control" => 44,
      "Telnet Suppress Local Echo" => 45,
      "Telnet Start TLS" => 46,
      "KERMIT" => 47,
      "SEND-URL" => 48,
      "FORWARD_X" => 49,
      "TELOPT PRAGMA LOGON" => 138,
      "TELOPT SSPI LOGON" => 139,
      "TELOPT PRAGMA HEARTBEAT" => 140,
      "Extended-Options-List" => 255
    }

  def reverse_options,
    do:
      options()
      |> Enum.map(fn {key, value} -> {value, key} end)
      |> Map.new()
end
