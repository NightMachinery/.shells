package main

import (
	"crypto/tls"
	"crypto/x509"
	"errors"
	"io"
	"net"
	"strings"
	"syscall"
)

// brishzq.zsh exits with curl's status when the request itself fails, so
// these are curl's exit codes (see `man curl`, EXIT CODES).
const (
	curlUnsupportedProtocol = 1
	curlMalformedURL        = 3
	curlResolve             = 6
	curlConnect             = 7
	curlPartial             = 18
	curlTimeout             = 28
	curlSSLConnect          = 35
	curlTooManyRedirects    = 47
	curlEmptyReply          = 52
	curlSend                = 55
	curlRecv                = 56
	curlPeerCert            = 60
	curlProxyError          = 97
)

var errTooManyRedirects = errors.New("too many redirects")

// curlExitCode maps a Go transport error to the exit status curl gives for
// the same failure. readingBody is true for an error while reading the
// reply body, after its headers arrived.
func curlExitCode(err error, readingBody bool) int {
	var dnsErr *net.DNSError
	var netErr net.Error
	var unknownCA x509.UnknownAuthorityError
	var hostErr x509.HostnameError
	var certErr x509.CertificateInvalidError
	var verifyErr *tls.CertificateVerificationError
	var recordErr tls.RecordHeaderError
	var opErr *net.OpError

	switch {
	case errors.Is(err, errTooManyRedirects):
		return curlTooManyRedirects
	case errors.Is(err, errUnsupportedProxy):
		// curl: "Unsupported proxy scheme", CURLE_COULDNT_CONNECT.
		return curlConnect
	case errors.As(err, &dnsErr):
		return curlResolve
	case errors.Is(err, syscall.ECONNREFUSED),
		errors.Is(err, syscall.ENETUNREACH),
		errors.Is(err, syscall.EHOSTUNREACH),
		errors.Is(err, syscall.EADDRNOTAVAIL):
		return curlConnect
	case errors.As(err, &netErr) && netErr.Timeout():
		return curlTimeout
	case errors.As(err, &unknownCA), errors.As(err, &hostErr),
		errors.As(err, &certErr), errors.As(err, &verifyErr):
		return curlPeerCert
	case errors.As(err, &recordErr):
		return curlSSLConnect
	case errors.Is(err, syscall.EPIPE):
		return curlSend
	case errors.Is(err, syscall.ECONNRESET):
		return curlRecv
	}
	msg := err.Error()
	switch {
	case strings.Contains(msg, "socks connect"):
		// The SOCKS proxy answered but could not connect us.
		return curlProxyError
	case strings.Contains(msg, "unsupported protocol scheme"):
		return curlUnsupportedProtocol
	case strings.Contains(msg, "invalid URL"), strings.Contains(msg, "missing protocol scheme"),
		strings.Contains(msg, "no Host in request URL"), strings.Contains(msg, "invalid port"),
		strings.Contains(msg, "invalid character"):
		return curlMalformedURL
	case strings.Contains(msg, "tls:"):
		return curlSSLConnect
	}
	if readingBody {
		if errors.Is(err, io.ErrUnexpectedEOF) || errors.Is(err, io.EOF) {
			return curlPartial
		}
		return curlRecv
	}
	if errors.Is(err, io.EOF) || errors.Is(err, io.ErrUnexpectedEOF) {
		// The server closed the connection without a reply.
		return curlEmptyReply
	}
	if errors.As(err, &opErr) && opErr.Op == "dial" {
		return curlConnect
	}
	return curlRecv
}
