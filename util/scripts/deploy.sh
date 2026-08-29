#!/usr/bin/env bash

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DRY_RUN=0
PLATFORM=""

info() { printf '[OK] %s\n' "$*"; }
warn() { printf '[INFO] %s\n' "$*"; }
err() { printf '[ERROR] %s\n' "$*" >&2; }

usage() {
  printf 'Usage: %s [--dry-run] [--platform macos|linux]\n' "$0"
}

detect_platform() {
  case "$OSTYPE" in
    darwin*) printf 'macos\n' ;;
    linux*) printf 'linux\n' ;;
    *) err "Unsupported OS: $OSTYPE"; return 1 ;;
  esac
}

validate_relative_path() {
  case "$2" in
    ""|/*|..|../*|*/../*|*/..) err "Invalid $1 path: $2"; return 1 ;;
  esac
}

next_backup_path() {
  local target="$1"
  local stamp candidate sequence

  stamp="$(date +%Y%m%d_%H%M%S)"
  candidate="$target.backup.$stamp"
  sequence=1
  while [ -e "$candidate" ] || [ -L "$candidate" ]; do
    candidate="$target.backup.$stamp.$sequence"
    sequence=$((sequence + 1))
  done
  printf '%s\n' "$candidate"
}

deploy_link() {
  local source_rel="$1"
  local target_rel="$2"
  local name="$3"
  local source="$DOTFILES_DIR/$source_rel"
  local target="$HOME/$target_rel"
  local backup=""

  validate_relative_path source "$source_rel"
  validate_relative_path target "$target_rel"

  if [ ! -e "$source" ]; then
    err "Source not found: $source"
    return 1
  fi

  if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
    info "$name already linked correctly"
    return
  fi

  if [ "$DRY_RUN" -eq 1 ]; then
    if [ -e "$target" ] || [ -L "$target" ]; then
      warn "Would back up $name"
    fi
    info "Would link $name"
    return
  fi

  mkdir -p "$(dirname "$target")"
  if [ -e "$target" ] || [ -L "$target" ]; then
    backup="$(next_backup_path "$target")"
    warn "Backing up $name to $backup"
    mv "$target" "$backup"
  fi

  if ! ln -s "$source" "$target"; then
    [ -n "$backup" ] && mv "$backup" "$target"
    err "Could not link $name"
    return 1
  fi
  info "Linked $name"
}

deploy_manifest() {
  local manifest="$1"
  local source target name extra

  [ -f "$manifest" ] || return
  while IFS=$'\t' read -r source target name extra; do
    [ -z "$source" ] && continue
    case "$source" in \#*) continue ;; esac
    if [ -z "$target" ] || [ -z "$name" ] || [ -n "$extra" ]; then
      err "Invalid manifest row in $manifest: $source"
      return 1
    fi
    deploy_link "$source" "$target" "$name"
  done < "$manifest"
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --dry-run) DRY_RUN=1 ;;
    --platform)
      shift
      [ "$#" -gt 0 ] || { err "--platform needs a value"; usage; exit 1; }
      PLATFORM="$1"
      ;;
    -h|--help) usage; exit ;;
    *) err "Unknown argument: $1"; usage; exit 1 ;;
  esac
  shift
done

[ -n "$PLATFORM" ] || PLATFORM="$(detect_platform)"
case "$PLATFORM" in
  macos|linux) ;;
  *) err "Unsupported platform: $PLATFORM"; exit 1 ;;
esac

printf 'Deploying shared and %s configuration\n\n' "$PLATFORM"
deploy_manifest "$DOTFILES_DIR/util/links.tsv"
deploy_manifest "$DOTFILES_DIR/$PLATFORM/links.tsv"
printf '\n'
info "Deployment complete"
