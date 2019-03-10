function readFd(fd)
  assert(fd)
  local content = fd:read("*a")
  fd:close()
  return content
end

function readFile(file)
  return readFd(io.open(file, "rb"))
end

function readCmd(cmd)
  return readFd(io.popen(cmd))
end

root = {
  -- Serve the content of a string
  ["luaversion"] = _VERSION,

  -- Serve the content of a file
  ["cpu"] = readFile("/proc/cpuinfo"),

  -- Serve command output.
  ["dmesg"] = readCmd("dmesg"),

  -- Allow glob patterns in file names.
  ["config/*"] = "always the same config",
}

return root
