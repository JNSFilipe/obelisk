package obelisk

import (
	"fmt"
	"os"
	"github.com/fatih/color"
)

type LogLevel uint

const (
	INFO LogLevel = iota
	WARNING
	ERROR
	FATAL
)

func Log(level LogLevel, format string, args ...interface{}) {
	var tag string

	switch level {
	case INFO:
		tag = color.New(color.FgGreen, color.Bold).SprintfFunc()("[INFO]")
	case WARNING:
		tag = color.New(color.FgYellow, color.Bold).SprintfFunc()("[WARNING]")
	case ERROR:
		tag = color.New(color.FgRed, color.Bold).SprintfFunc()("[ERROR]")
	case FATAL:
		tag = color.New(color.BgRed, color.FgBlack, color.Bold).SprintfFunc()("[FATAL]")
	default:
		fmt.Fprintf(os.Stderr, "[Unknown]")
		panic("unreachable")
	}

	fmt.Fprintf(os.Stderr, "%s %s\n", tag, fmt.Sprintf(format, args...))
	if level == FATAL {
		panic("[OBELISK] LOG FATAL")
	}
}
