json4delphi
===========

JSON for Delphi, support for older versions of Delphi (6 or above)

Object-pascal native code, using classes only TList, TStrings and TStringList

Example:

```pascal
var
  Json: TJson;
  Str: String
begin
  Json := TJson.Create();

  //put
  Json.Put('field1', null);
  Json.Put('field2', True);
  Json.Put('field3', 3.14);
  Json.Put('field4', 'hello world');

  //another way
  Json['field5'].AsBoolean := False;
  Json['field6'].AsString := 'hello world';

  //object
  with Json['field7'].AsObject do
  begin
    Put('subfield1', 2.7182818284);
    Put('subfield2', 'json4delphi');
  end;

  //array
  with Json['field8'].AsArray do
  begin
    Put(6.6260755e-34);
    Put('The magic words are squeamish ossifrage');
  end;

  //get
  Str := Json['field4'].AsString;

  //parse
  Json.Parse('{"a":1}');

  //stringify
  Str := Json.Stringify;
end;
```
