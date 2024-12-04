package br.com.acbr.lib.comum.ini;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public @interface IniKeyAttribute {
    String value();
}
