package com.acbr.mail;

import java.util.HashMap;
import java.util.Map;

public enum MailAttachmentDisposition {
    Attachment(0),
    Inline(1);
    
    private static final Map<Integer, MailAttachmentDisposition> map;
    private final int enumValue;

    static {
        map = new HashMap<>();
        for (MailAttachmentDisposition value : MailAttachmentDisposition.values()) {
            map.put(value.asInt(), value);
        }
    }

    public static MailAttachmentDisposition valueOf(int value) {
        return map.get(value);
    }

    private MailAttachmentDisposition(int id) {
        this.enumValue = id;
    }

    public int asInt() {
        return enumValue;
    }
}
