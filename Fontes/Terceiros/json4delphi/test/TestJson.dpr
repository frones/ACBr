program TestJson;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Jsons in '..\src\Jsons.pas';

procedure RunTest;
var
  Json: TJson;
  Str: String;
begin
  Json := TJson.Create;

  try

    Json.Put('null-field', null);
    Json.Put('boolean-field-true', True);

    Json['boolean-field-false'].AsBoolean := not Json.Get('boolean-field-true').AsBoolean;
    Json['number-field'].AsNumber := 3.1415926535;
    Json['number-field-integer'].AsInteger := Json['number-field'].AsInteger;
    Json['string-field'].AsString := 'Hello world';

    with Json.Put('array-field', empty).AsArray do
    begin
      Put(empty);
      Put(null);
      Put(False);
      Put(True);
      Put(299792458);
      Put(2.7182818284);
      Put('The magic words are squeamish ossifrage');
      with Put(empty).AsObject do
      begin
        Put('array-object-field-1', null);
        Put('array-object-field-2', 'json4delphi');
      end;
    end;
    with Json.Put('object-field', empty).AsObject do
    begin
      Put('object-field-1', True);
      Put('object-field-2', 6.6260755e-34);
    end;
    Str := Json.Stringify;
    Writeln(Str);
    Json.Clear;
    Json.Parse(Str);
  finally
    Json.Free;
  end;
end;

begin
  RunTest;
  ReadLn;
end.
