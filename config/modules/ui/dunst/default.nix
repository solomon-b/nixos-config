{ ... }:
{
  primary-user.home-manager.services.dunst = {
    enable = true;
    settings = {
      global = {
        font = "Monospace 6";
        follow = "mouse";
        geometry = "300x5+1610+30";
        indicate_hidden = "yes";
        shrunk = "no";
        notification_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        frame_width = 1;
        separator_color = "frame";
        markup = "full";
        format = "<b>%s</b>\n%b";
        alignment = "left";
        show_age_threshold = 60;
        word_wrap = "yes";
        ellipsize = "middle";
        ignore_newline = "no";
        stack_duplicates = true;
        hide_duplicate_count = false;
        show_indicators = "yes";
        icon_position = "left";
        max_icon_size = 32;
        sticky_history = "yes";
        history_length = 20;
        dmenu = "/usr/bin/dmenu -p dunst";
        browser = "/usr/bin/firefox -new-tab";
        always_run_script = true;
        title = "Dunst";
        class = "Dunst";
        startup_notification = false;
        force_xinerama = false;
      };

      experimental.per_monitor_dpi = false;

      urgency_low = {
        frame_color = "#99cc99";
        background = "#2d2d2d";
        foreground = "#cccccc";
        timeout = 10;
      };

      urgency_normal = {
        frame_color = "#f99157";
        background = "#2d2d2d";
        foreground = "#cccccc";
        timeout = 10;
      };

      urgency_critical = {
        #frame_color = "#f2777a";
        frame_color = "#ff0000";
        background = "#2d2d2d";
        foreground = "#cccccc";
        timeout = 0;
      };
    };
  };
}
