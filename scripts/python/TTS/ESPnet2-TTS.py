#!/usr/bin/env python

# Adapted from https://colab.research.google.com/github/espnet/notebook/blob/master/espnet2_tts_realtime_demo.ipynb#scrollTo=MuhqhYSToxl7
# Installation:
# pip3 install torch torchvision espnet==0.9.7 parallel_wavegan==0.4.8 espnet_model_zoo
#
# Usage:
# echo some text | ESPnet2-TTS.py > output.wav

fs, lang = 22050, "English"

##
# `espnet_model_zoo_query`
# https://github.com/espnet/espnet_model_zoo/blob/master/espnet_model_zoo/table.csv
# https://zenodo.org/communities/espnet/search?page=1&size=20&q=&sort=mostviewed
# jsut: Japanese
# csmsc: Mandarin

# tag = "kan-bayashi/ljspeech_tacotron2"
# tag = "kan-bayashi/ljspeech_fastspeech"
# tag = "kan-bayashi/ljspeech_fastspeech2"
tag = "kan-bayashi/ljspeech_conformer_fastspeech2"

# tag = "kan-bayashi/ljspeech_tts_train_tacotron2_raw_phn_tacotron_g2p_en_no_space_train.loss.best" # slower speech, more old lady like

# tag = "kan-bayashi/ljspeech_tts_train_fastspeech_raw_phn_tacotron_g2p_en_no_space_train.loss.best" # has some subtle background noise/echo?
##
# vocoder_tag = "ljspeech_parallel_wavegan.v1"
vocoder_tag = "ljspeech_full_band_melgan.v2"
# vocoder_tag = "ljspeech_multi_band_melgan.v2"
##

import sys
import time
import torch
from espnet_model_zoo.downloader import ModelDownloader
from espnet2.bin.tts_inference import Text2Speech
from parallel_wavegan.utils import download_pretrained_model
from parallel_wavegan.utils import load_model
d = ModelDownloader()
text2speech = Text2Speech(
    **d.download_and_unpack(tag),
    # device="cuda",
    device="cpu",
    # Only for Tacotron 2
    threshold=0.5,
    minlenratio=0.0,
    maxlenratio=10.0,
    use_att_constraint=False,
    backward_window=1,
    forward_window=3,
    # Only for FastSpeech & FastSpeech2
    speed_control_alpha=1.0,
)
text2speech.spc2wav = None  # Disable griffin-lim
# NOTE: Sometimes download is failed due to "Permission denied". That is
#   the limitation of google drive. Please retry after serveral hours.

# vocoder = load_model(download_pretrained_model(vocoder_tag)).to("cuda").eval()
vocoder = load_model(download_pretrained_model(vocoder_tag)).eval()
vocoder.remove_weight_norm()

# print(f"Input Phase Initiated", file=sys.stderr)
## decide the input sentence by yourself
# print(f"Input your favorite sentence in {lang}.", file=sys.stderr)
# x = input()
x = sys.stdin.read()

# synthesis
with torch.no_grad():
    start = time.time()
    wav, c, *_ = text2speech(x)
    wav = vocoder.inference(c)
rtf = (time.time() - start) / (len(wav) / fs)
# print(f"RTF = {rtf:5f}", file=sys.stderr)

# let us listen to generated samples
from IPython.display import display, Audio
audio = Audio(wav.view(-1).cpu().numpy(), rate=fs)

# display(audio) # works in jupyter

# with open('./test.wav', 'wb') as f: # works but we now output to stdout
    # f.write(audio.data)
    # 
sys.stdout.buffer.write(audio.data)
