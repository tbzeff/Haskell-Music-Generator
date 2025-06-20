# myaudiogenerator

A Haskell-based music generation framework designed for programmatic audio synthesis and composition. Currently, it supports waveform synthesis (sine, square, saw), amplitude envelopes, basic filtering, chord construction, and multi-track layering.

This is a work-in-progress project with plans to expand into real-time audio generation and MIDI input support.

## Features

- Generate sine, square, and sawtooth waveforms
- Compose music with notes, chords, and rests
- Apply amplitude envelopes
- Combine layers to build complex tracks
- Save and play raw waveform audio
- Includes a demo track: **HalloweenMusic**

## Requirements

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [ffplay (part of FFmpeg)](https://ffmpeg.org/ffplay.html) for audio playback

Make sure `ffplay` is available in your system's PATH.

## Getting Started

### Clone and Setup

```bash
git clone https://github.com/tbzeff/myHaskell-Music-Generator.git
cd myaudiogenerator
stack setup
stack build
```

### Run the Example Song

The project includes a generative song in `AmbientMusic.hs`. To play it:

```bash
stack run
```

This will generate the audio signal and play it using `ffplay`.

### File Output

The raw waveform data is saved to `output.bin`. You can modify `Utility.hs` to save `.wav` files via `saveWave`.

## Project Structure

- `Main.hs`: Entry point, plays the example song
- `AmbientMusic.hs`: Composed demo song
- `HalloweenMusic.hs`: Composed demo song
- `Composition.hs`: Note and track creation utilities
- `Freq.hs`: Waveform generation
- `Envelopes.hs`: ADSR and pitch envelope support
- `Filters.hs`: FIR/IIR filter implementation
- `Utility.hs`: Audio file writing and playback
- `Defaults.hs`: Common types, constants, and configuration

## Roadmap

- [ ] Real-time audio playback with MIDI input
- [ ] Expand envelope types (e.g., exponential, custom curves)
- [ ] Percussive sound synthesis
- [ ] Visual GUI for parameter editing
- [ ] Interactive REPL for live composition

---

## 👤 Author

Taylor Bleizeffer
- **https://www.taylorbleizeffer.com**
- **https://github.com/tbzeff**

Made with ❤️ and Haskell.



