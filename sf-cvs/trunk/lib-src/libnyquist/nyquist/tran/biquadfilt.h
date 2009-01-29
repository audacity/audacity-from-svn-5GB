sound_type snd_make_biquadfilt(sound_type s, double b0, double b1, double b2, double a1, double a2, double z1init, double z2init);
sound_type snd_biquadfilt(sound_type s, double b0, double b1, double b2, double a1, double a2, double z1init, double z2init);
    /* LISP: (snd-biquad SOUND ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM) */
    /* note: snd_biquadfilt is used to avoid naming conflict with STK's BiQuad 
     *       class, but the Lisp function name is SND-BIQUAD.
     */
