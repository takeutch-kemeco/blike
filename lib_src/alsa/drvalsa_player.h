#include <glib.h>
#include <asoundlib.h>

#ifndef __DRVALSA_PLAYER_H__
#define __DRVALSA_PLAYER_H__

extern snd_pcm_sframes_t	drvalsa_period_size;
extern const snd_pcm_format_t	drvalsa_format;
extern const guint		drvalsa_channels;



extern gint drvalsa_init_player(void);
extern void drvalsa_play_pcm(gint16* left_pcm, gint16* right_pcm, gint pcm_len);
extern void drvalsa_close_player(void);

#endif // __DRVALSA_PLAYER_H__
