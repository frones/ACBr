unit SrvOD.Middlewares;

{$MODE DELPHI}{$H+}


interface

uses
  Horse,
  ACBrOpenDeliverySchemaClasses,
  ACBrJSON,
  SysUtils,
  Classes;

procedure HorseACBrJSON(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
procedure HorseODError(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);

implementation

procedure HorseACBrJSON(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LJSON: TACBrJSON;
begin
  try
    Next;
  finally
    if (Res.Content <> nil) and (Res.Content is TACBrJSON) then
    begin
      LJSON := TACBrJSON(Res.Content);
      Res.RawWebResponse.Content := LJSON.ToJSON;
      Res.RawWebResponse.ContentType := 'application/json; charset=UTF-8';
    end;
  end;
end;

procedure HorseODError(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  LStatus: Integer;
  LMessage: string;
  LJSON: TACBrJSONObject;
begin
  try
    Next;
  except
    on E: EHorseException do
    begin
      LStatus := Integer(E.Status);
      LMessage := E.Error;
      LJSON := TACBrJSONObject.Create
                 .AddPair('status', LStatus)
                 .AddPair('title', LMessage);
      Res.Send<TACBrJSONObject>(LJSON).Status(LStatus);
    end;

    on E: Exception do
    begin
      LStatus := Res.Status;
      if LStatus < Integer(THTTPStatus.BadRequest) then
        LStatus := Integer(THTTPStatus.InternalServerError);
      LMessage := E.Message;
      LJSON := TACBrJSONObject.Create
                 .AddPair('status', LStatus)
                 .AddPair('title', LMessage);
      Res.Send<TACBrJSONObject>(LJSON).Status(LStatus);
    end;
  end;
end;

end.
