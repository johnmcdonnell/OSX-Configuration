"use strict";
XML.ignoreWhitespace = false;
XML.prettyPrinting = false;
var INFO =
<plugin name="cookies" version="1.0.1"
        href="http://dactyl.sf.net/pentadactyl/plugins#cookies-plugin"
        summary="Cookie manager"
        xmlns={NS}>
    <author email="maglione.k@gmail.com">Kris Maglione</author>
    <license href="http://opensource.org/licenses/mit-license.php">MIT</license>
    <project name="Pentadactyl" min-version="1.0"/>
    <p>
        This plugin helps managing cookies and permissions for specific sites.
        Cookies may be enabled, disabled, or enabled for the current session.
        Additionally, cookies for a given domain may be listed or cleared on
        demand.
    </p>
    <item>
        <tags>:cookies :ck</tags>
        <spec>:cookies <a>host</a> <oa>action</oa> {UTF8("…")}</spec>
        <description>
            <p>
                Manage cookies for <a>host</a>. Additionally, the completion
                list will show you information about the cookies and
                permissions for the current page.
            </p>

            <p>Available actions:</p>

            <dl>
                <dt>unset</dt>            <dd>Unset special permissions for <a>host</a></dd>
                <dt>allow</dt>            <dd>Allow cookies from <a>host</a></dd>
                <dt>deny</dt>             <dd>Deny cookies from <a>host</a></dd>
                <dt>session</dt>          <dd>Allow cookies from <a>host</a> for the current session</dd>
                <dt>list</dt>             <dd>List all cookies for <a>host</a></dd>
                <dt>clear</dt>            <dd>Clear all cookies for <a>host</a></dd>
                <dt>clear-persistent</dt> <dd>Clear all persistent cookies for <a>host</a></dd>
                <dt>clear-session</dt>    <dd>Clear all session cookies for <a>host</a></dd>
            </dl>

            <p>
                If no <oa>action</oa> is given, the value of <o>cookies</o> is used.
            </p>

            <example>:map c :cookies <k name="Tab"/></example>
        </description>
    </item>
    <item>
        <tags>'ck' 'cookies'</tags>
        <spec>'cookies' 'ck'</spec>
        <type>stringlist</type> <default>session</default>
        <description>
            <p>
                The default action for the <ex>:cookies</ex> command.
            </p>
        </description>
    </item>
</plugin>;

services.add("cookies",     "@mozilla.org/cookiemanager;1",                 [Ci.nsICookieManager, Ci.nsICookieManager2, Ci.nsICookieService]);
services.add("permissions", "@mozilla.org/permissionmanager;1",             Ci.nsIPermissionManager);
services.add("tld",         "@mozilla.org/network/effective-tld-service;1", Ci.nsIEffectiveTLDService);

function endsWith(str, value)
    str.length >= value.length && str.lastIndexOf(value) == str.length - value.length;

var PERMS = {
    unset:   0,
    allow:   1,
    deny:    2,
    session: 8,
};
var UNPERMS = array.toObject([[v, k] for ([k, v] in Iterator(PERMS))]);
var COMMANDS = {
    unset:   "Unset",
    allow:   "Allowed",
    deny:    "Denied",
    session: "Allowed for the current session",
    list:    "List all cookies for domain",
    clear:   "Clear all cookies for domain",
    "clear-persistent": "Clear all persistent cookies for domain",
    "clear-session":    "Clear all session cookies for domain",
};

function setperms(host, perm) {
    let uri = util.createURI(host);
    services.permissions.remove(uri, "cookie");
    services.permissions.add(uri, "cookie", PERMS[perm]);
}

function get(host) {
    try {
        let uri = util.createURI(host);
        return UNPERMS[services.permissions.testPermission(uri, "cookie")];
    }
    catch (e) {
        return "unset";
    }
}

function iterCookies(host) {
    for (let c in iter(services.cookies))
        if (c.QueryInterface(Ci.nsICookie2).rawHost == host ||
            endsWith(c.host, host))
            yield c;
}

function subdomains(host) {
    if (/(^|\.)\d+$|:.*:/.test(host))
        // IP address or similar
        return [host];

    let base = host.replace(/.*\.(.+?\..+?)$/, "$1");
    try {
        base = services.tld.getBaseDomainFromHost(host);
    }
    catch (e) {}

    let ary = host.split(".");
    ary = [ary.slice(i).join(".") for (i in util.range(ary.length - 1, 0, -1))];
    return ary.filter(function (h) h.length >= base.length);
}

function completeHosts(context) {
    function desc(host) {
        let count = [0, 0];
        for (let c in iterCookies(host))
            count[c.isSession + 0]++;
        return <>{COMMANDS[get(host)]} (session: {count[1]} persistent: {count[0]})</>;
    }

    let res = [], seen = {};
    (function rec(frame) {
        try {
            res = res.concat(subdomains(frame.location.host));
        } catch (e) {}
        Array.forEach(frame.frames, rec);
    })(content);
    if (context.filter && !res.some(function (host) host.indexOf(context.filter) >= 0))
        res.push(context.filter);

    context.anchored = false;
    context.compare = CompletionContext.Sort.unsorted;
    context.keys = { text: util.identity, description: desc };
    context.completions = res.filter(function (h) !set.add(seen, h));
}
function completePerms(context) {
    context.keys = { text: "0", description: function ([k, v]) COMMANDS[v] };
    context.completions = iter(PERMS);
}

options.add(["cookies", "ck"],
    "The default mode for newly added cookie permissions",
    "stringlist", "session",
    {
        completer: completePerms,
        validator: Options.validateCompleter,
    });
commands.addUserCommand(["cookies", "ck"],
    "Change cookie permissions for sites.",
    function (args) {
        let host = args.shift();
        let session = true;
        if (!args.length)
            args = options["cookies"];

        for (let [,cmd] in Iterator(args))
            switch (cmd) {
            case "clear":
                for (let c in iterCookies(host))
                    services.cookies.remove(c.host, c.name, c.path, false);
                break;
            case "clear-persistent":
                session = false;
            case "clear-session":
                for (let c in iterCookies(host))
                    if (c.isSession == session)
                        services.cookies.remove(c.host, c.name, c.path, false);
                return;

            case "list":
                dactyl.echo(template.tabular(
                    ["Host", "Session", "Path", "Value"], ["padding-right: 1em", "padding-right: 1em", "padding-right: 1em"],
                    ([c.host,
                      <span highlight={c.isSession ? "Enabled" : "Disabled"}>{c.isSession ? "session" : "persistent"}</span>,
                      c.path,
                      c.value]
                      for (c in iterCookies(host)))));
                return;
            default:
                dactyl.assert(cmd in PERMS, "Invalid argument");
                setperms(host, cmd);
            }
    }, {
        argCount: "+",
        completer: function (context, args) {
            switch (args.completeArg) {
            case 0: completeHosts(context); break;
            case 1: context.completions = COMMANDS; break;
            }
        },
    }, true);

/* vim:se sts=4 sw=4 et: */
