{application, chatapp,
 [{description, "Chat Services"},
    {vsn, "1.0.0"},
    {modules, [chatapp, chat_sup, chat_server, chat_user1, chat_user2]},
    {registered, [cha_tapp, chat_sup]},
    {applications, [kernel, stdlib]},
    {mod, {chat_app,[]}}]}.