{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrDeviceRaw;

interface

uses
  Classes, SysUtils,
  ACBrDeviceClass, ACBrBase;

type

  { TACBrDeviceRaw }

  TACBrDeviceRaw = class(TACBrDeviceClass)
  private
    function GetPrinterIndex: Integer;
    function GetNomeDocumento: String;

  protected
  public
    procedure Conectar(const APorta: String; const ATimeOutMilissegundos: Integer); override;
    procedure EnviaString(const AString: AnsiString); override;

    procedure AcharPortasRAW(const AStringList: TStrings);
    function GetLabelPrinterIndex(APrinterName: String): Integer;
  end;

implementation

uses
  StrUtils,
  {$If defined(VisualCLX)}
    QPrinters,
  {$ElseIf defined(FMX)}
    FMX.Printer,
    {$IfDef MSWINDOWS}
      Winapi.WinSpool, Winapi.Windows,
    {$EndIf}
  {$ElseIf defined(FPC)}
    Printers,
    {$IfNDef NOGUI}
      OSPrinters,
    {$EndIf}
  {$Else}
    Printers, WinSpool, Windows,
  {$IfEnd}
  {$IF defined(POSIX)}
    Posix.Unistd,
  {$IfEnd}
  ACBrUtil.Strings, ACBrConsts, ACBrDevice;


{ TACBrDeviceRaw }

procedure TACBrDeviceRaw.AcharPortasRAW(const AStringList: TStrings);
var
  PrinterSys: TPrinter;
  i: Integer;
begin
  PrinterSys := Printer;
  if Assigned(PrinterSys) then
    For i := 0 to PrinterSys{$IfNDef FMX}.Printers{$EndIf}.Count-1 do
      AStringList.Add('RAW:'+PrinterSys.Printers[i]{$IfDef FMX}.Title{$EndIf});
end;

procedure TACBrDeviceRaw.Conectar(const APorta: String;
  const ATimeOutMilissegundos: Integer);
begin
  inherited;
  GetPrinterIndex;
end;

function TACBrDeviceRaw.GetLabelPrinterIndex(APrinterName: String): Integer;
var
 PrinterSys: TPrinter;
 {$IfDef FMX}
  i: Integer;
 {$EndIf}
begin
  Result := -1;
  PrinterSys := Printer;
  if Assigned(PrinterSys) then
  begin
    {$IfDef FMX}
     for i := 0 to PrinterSys.Count - 1 do
       if AnsiContainsText(PrinterSys.Printers[i].Title, APrinterName) then
         Exit(i);
    {$Else}
     Result := PrinterSys.Printers.IndexOf(APrinterName)
    {$EndIf}
  end;
end;

function TACBrDeviceRaw.GetPrinterIndex: Integer;
var
  PrnName: String;
  PrnIndex: Integer;
  PrinterSys: TPrinter;

  function RetornaNome(const ANome: string): string;
  begin
    Result := Copy(ANome, 3, Length(ANome));
    Result := Copy(Result, pos('\', Result) + 1, Length(Result));
  end;

  function RetornaPorta(PrinterSys: TPrinter; PrnName: String): Integer;
  {$IfDef MSWINDOWS}
   var
     I: Integer;
     VName: String;
  {$EndIf}
  begin
    Result := GetLabelPrinterIndex(PrnName);

    {$IfDef MSWINDOWS}
     if Result < 0 then
     begin
       for I := 0 to Pred(Printer{$IfNDef FMX}.Printers{$EndIf}.Count) do
       begin
         VName := PrinterSys.Printers[I]{$IfDef FMX}.Title{$EndIf};
         if pos('\\', Copy(VName, 1, 2)) > 0 then
         begin
           if SameText(PrnName, RetornaNome(VName)) then
           begin
             Result := I;
             Break;
           end;
         end;
       end;
     end;
    {$EndIf}
  end;

begin
  GravaLog('GetPrinterIndex');

  PrnName := fpPorta;
  if (copy(UpperCase(PrnName), 1, 4) = 'RAW:') then
    PrnName := copy(PrnName, 5, Length(PrnName)) ;

  PrinterSys := Printer;

  if Assigned(PrinterSys) then
  begin
    if (PrnName = '*') then
    begin
      {$IfDef FMX}
       PrnIndex := 0;
       if Assigned(PrinterSys.ActivePrinter) then
       begin
         PrnName := PrinterSys.ActivePrinter.Title;
         PrnIndex := RetornaPorta(PrinterSys, PrnName);
       end
       else
       begin
         if (PrinterSys.Count = 0) then
           DoException( Exception.Create(ACBrStr(cACBrDeviceSemImpressoraPadrao)));
       end;
      {$Else}
       PrnIndex := PrinterSys.PrinterIndex;
       if (PrnIndex < 0) then
       begin
         if PrinterSys.Printers.Count > 0 then
           PrnIndex := 0
         else
           DoException( Exception.Create(ACBrStr(cACBrDeviceSemImpressoraPadrao)));
       end;
      {$EndIf}
    end
    else
    begin
      PrnIndex := RetornaPorta(PrinterSys, PrnName);
      if (PrnIndex < 0) then
        DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceImpressoraNaoEncontrada), [PrnName]));
    end;
  end
  else
    DoException( Exception.Create(ACBrStr(cACBrDeviceSemImpressoraPadrao)));

  Result := PrnIndex;
  GravaLog('  '+IntToStr(Result));
end;


{$IfDef FPC}
 procedure TACBrDeviceRaw.EnviaString(const AString: AnsiString);
 var
   PrinterSys: TPrinter;
   PrnIndex: Integer;
   Written: integer;
   OldRawMode: Boolean;
 begin
   GravaLog('  TACBrDeviceRaw.EnviaStringFPC');

   PrinterSys := Printer;
   if Assigned(PrinterSys) then
   begin
     PrnIndex := GetPrinterIndex;
     PrinterSys.PrinterIndex := PrnIndex;
     PrinterSys.Title := GetNomeDocumento;

     OldRawMode := PrinterSys.RawMode;
     PrinterSys.RawMode := True;
     try
       PrinterSys.BeginDoc;
       Written := 0;
       PrinterSys.Write(AString[1], Length(AString), Written);
       PrinterSys.EndDoc;
     finally
       PrinterSys.RawMode := OldRawMode;
     end;
   end;
 end;
{$Else}
 {$IfDef MSWINDOWS}
  procedure TACBrDeviceRaw.EnviaString(const AString: AnsiString);
  var
    PrinterSys: TPrinter;
    PrnIndex: Integer;
    PrnName: String;
    HandlePrn: THandle;
    N: DWORD;
    DocName: String;
    DocInfo1: TDocInfo1;
  begin
    GravaLog('  TACBrDeviceRaw.EnviaStringWindows');
    PrinterSys := Printer;

    if Assigned(PrinterSys) then
    begin
      PrnIndex := GetPrinterIndex;
      PrnName := PrinterSys.Printers[PrnIndex]{$IfDef FMX}.Title{$EndIf};
      if not OpenPrinter(PChar(PrnName), HandlePrn, nil) then
        DoException( Exception.CreateFmt(ACBrStr(cACBrDeviceImpressoraNaoEncontrada), [PrnName]));

      try
        with DocInfo1 do
        begin
          DocName := GetNomeDocumento;
          pDocName := PChar(DocName);
          pOutputFile := Nil;
          pDataType := 'RAW';
        end;

        StartDocPrinter(HandlePrn, 1, @DocInfo1);
        WritePrinter(HandlePrn, PAnsiChar(AString), Length(AString), N);
        EndPagePrinter(HandlePrn);
        EndDocPrinter(HandlePrn);
      finally
        ClosePrinter(HandlePrn);
      end;
    end;
  end;
 {$Else}  // Delphi, Linux
  procedure TACBrDeviceRaw.EnviaString(const AString: AnsiString);
  var
    PrnIndex: Integer;
    F: TextFile;
    {$IfDef FMX}
     PrinterSys: TPrinter;
    {$EndIf}
  begin
    GravaLog('  TACBrDeviceRaw.EnviaStringLinux');
    PrnIndex := GetPrinterIndex;

    {$IfDef FMX}
     PrinterSys := Printer;
     if Assigned(PrinterSys) then
       if (PrnIndex < PrinterSys.Count) then
         PrinterSys.ActivePrinter := PrinterSys.Printers[PrnIndex];
    {$EndIf}

    AssignPrn(F);
    try
      ReWrite(F);
      Write(F,AString);
    finally
      CloseFile(F);
    end;
  end;
 {$EndIf}
{$EndIf}

function TACBrDeviceRaw.GetNomeDocumento: String;
begin
  Result := TACBrDevice(fpOwner).NomeDocumento;
end;

end.


