{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Isaque Pinheiro                                 }
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

unit ACBrPAF;

interface

uses
   SysUtils, Classes, DateUtils, ACBrBase, math,
   {$IFNDEF NOGUI}
    {$IFDEF FPC}LResources,{$ENDIF}
    {$IF DEFINED(CLX)}
       QForms,
    {$ELSEIF DEFINED(FMX)}
       FMX.Forms,
    {$ELSE}
       Forms,
    {$IFEND}
//    {$IFDEF CLX} QForms, {$ELSE} Forms, {$ENDIF}
   {$ENDIF}
   ACBrTXTClass, ACBrUtil, ACBrEAD, ACBrAAC,
   ACBrPAF_A_Class,
   ACBrPAF_B_Class,
   ACBrPAF_C_Class,
   ACBrPAF_D_Class,
   ACBrPAF_E_Class,
   ACBrPAF_F_Class,
   ACBrPAF_G_Class,
   ACBrPAF_H_Class,
   ACBrPAF_J_Class,
   ACBrPAF_L_Class,
   ACBrPAF_M_Class,
   ACBrPAF_N_Class,
   ACBrPAF_P_Class,
   ACBrPAF_R_Class,
   ACBrPAF_S_Class,
   ACBrPAF_T_Class,
   ACBrPAF_TITP_Class,
   ACBrPAF_W_Class,
   ACBrPAF_U_Class,
   ACBrPAF_V_Class,
   ACBrPAF_Z_Class;

type

  // DECLARANDO O COMPONENTE - PAF-ECF:

  { TACBrPAF }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPAF = class(TACBrComponent)
  private
    FOnError: TErrorEvent;

    fsEADInterno : TACBrEAD ;
    fsEAD : TACBrEAD ;       /// Componente usado para AssinarArquivo com assinatura EAD.
    fsAAC : TACBrAAC ;       /// Componente usado para manter o Arq.Auxiliar Criptografado

    FPath: String;            // Path do arquivo a ser gerado
    FTrimString: boolean;     // Retorna a string sem espaços em branco iniciais e finais
    FAssinar : Boolean;       // Define se o arquivo gerado deve ser assinado
    FLinesBuffer: Integer;

    FPAF_A: TPAF_A;
    FPAF_B: TPAF_B;
    FPAF_C: TPAF_C;
    FPAF_D: TPAF_D;
    FPAF_E: TPAF_E;
    FPAF_F: TPAF_F;
    FPAF_G: TPAF_G;
    FPAF_H: TPAF_H;
    FPAF_J: TPAF_J;
    FPAF_L: TPAF_L;
    FPAF_M: TPAF_M;
    FPAF_N: TPAF_N;
    FPAF_P: TPAF_P;
    FPAF_R: TPAF_R;
    FPAF_S: TPAF_S;
    FPAF_T: TPAF_T;
    FPAF_TITP: TPAF_TITP;
    FPAF_W: TPAF_W;
    FPAF_U: TPAF_U;
    FPAF_V: TPAF_V;
    FPAF_Z: TPAF_Z;
    fsOnPAFCalcEAD: TACBrEADCalc;
    fsOnPAFGetKeyRSA : TACBrEADGetChave ;

    function GetPath: String;
    function GetTrimString: boolean;
    procedure SetLinesBuffer(AValue: Integer);
    procedure SetPath(const AValue: String);
    procedure SetTrimString(const Value: boolean);

    function AjustaNomeArquivo(Arquivo: String): String;

    function GetOnError: TErrorEvent; // Método do evento OnError
    procedure SetOnError(const Value: TErrorEvent); // Método SetError

    procedure SetEAD(const AValue: TACBrEAD);
    procedure SetAAC(const AValue: TACBrAAC);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override; // Create
    destructor Destroy; override; // Destroy

    // Métodos que escrevem o arquivo texto no caminho passado como parâmetro
    function SaveToFile_N(Arquivo: String): Boolean;
    function SaveToFile_TITP(Arquivo: String): Boolean;
    function SaveToFile_W(Arquivo: String): Boolean;
    function SaveToFile_Z(Arquivo: String): Boolean;
    function SaveToFile_V(Arquivo: String): Boolean;
    function SaveToFile_RegistrosPAF(Arquivo: String): Boolean;

    property PAF_A: TPAF_A read FPAF_A write FPAF_A;
    property PAF_B: TPAF_B read FPAF_B write FPAF_B;
    property PAF_C: TPAF_C read FPAF_C write FPAF_C;
    property PAF_D: TPAF_D read FPAF_D write FPAF_D;
    property PAF_E: TPAF_E read FPAF_E write FPAF_E;
    property PAF_F: TPAF_F read FPAF_F write FPAF_F;
    property PAF_G: TPAF_G read FPAF_G write FPAF_G;
    property PAF_H: TPAF_H read FPAF_H write FPAF_H;
    property PAF_J: TPAF_J read FPAF_J write FPAF_J;
    property PAF_L: TPAF_L read FPAF_L write FPAF_L;
    property PAF_M: TPAF_M read FPAF_M write FPAF_M;
    property PAF_N: TPAF_N read FPAF_N write FPAF_N;
    property PAF_P: TPAF_P read FPAF_P write FPAF_P;
    property PAF_R: TPAF_R read FPAF_R write FPAF_R;
    property PAF_S: TPAF_S read FPAF_S write FPAF_S;
    property PAF_T: TPAF_T read FPAF_T write FPAF_T;
    property PAF_TITP: TPAF_TITP read FPAF_TITP write FPAF_TITP;
    property PAF_W: TPAF_W read FPAF_W write FPAF_W;
    property PAF_U: TPAF_U read FPAF_U write FPAF_U;
    property PAF_V: TPAF_V read FPAF_V write FPAF_V;
    property PAF_Z: TPAF_Z read FPAF_Z write FPAF_Z;

    Function GetACBrEAD : TACBrEAD ;
    function AssinaArquivoComEAD(const Arquivo: String): Boolean;
  published
    property LinesBuffer  : Integer  read FLinesBuffer write SetLinesBuffer ;
    property Path         : String   read GetPath write SetPath ;
    property EAD          : TACBrEAD read fsEAD write SetEAD ;
    property AAC          : TACBrAAC read fsAAC write SetAAC ;

    property TrimString: Boolean read GetTrimString write SetTrimString
       default True ;
    property AssinarArquivo : Boolean read FAssinar write FAssinar
      default True ;

    property OnError: TErrorEvent  read GetOnError write SetOnError;
    property OnPAFCalcEAD: TACBrEADCalc read fsOnPAFCalcEAD
       write fsOnPAFCalcEAD;
    property OnPAFGetKeyRSA: TACBrEADGetChave read fsOnPAFGetKeyRSA
       write fsOnPAFGetKeyRSA;
  end;

implementation

Uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF MSWINDOWS}
  {$IFDEF COMPILER6_UP} StrUtils {$ELSE} ACBrD5 {$ENDIF} ;

{$IFNDEF FPC}
 {$R ACBrPAF.dcr}
{$ENDIF}

constructor TACBrPAF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPAF_A := TPAF_A.Create;
  FPAF_B := TPAF_B.Create;
  FPAF_C := TPAF_C.Create;
  FPAF_D := TPAF_D.Create;
  FPAF_E := TPAF_E.Create;
  FPAF_F := TPAF_F.Create;
  FPAF_G := TPAF_G.Create;
  FPAF_H := TPAF_H.Create;
  FPAF_J := TPAF_J.Create;
  FPAF_L := TPAF_L.Create;
  FPAF_M := TPAF_M.Create;
  FPAF_N := TPAF_N.Create(Self);
  FPAF_P := TPAF_P.Create;
  FPAF_R := TPAF_R.Create;
  FPAF_T := TPAF_T.Create;
  FPAF_S := TPAF_S.Create;
  FPAF_TITP := TPAF_TITP.Create;
  FPAF_W := TPAF_W.Create;
  FPAF_U := TPAF_U.Create;
  FPAF_V := TPAF_V.Create;
  FPAF_Z := TPAF_Z.Create;

  fsEADInterno     := nil;
  fsEAD            := nil;
  fsOnPAFGetKeyRSA := nil;
  fsOnPAFCalcEAD   := nil;

  FPath        := '';
  FTrimString  := True;
  FAssinar     := True;
  FLinesBuffer := 1000;
end;

destructor TACBrPAF.Destroy;
begin
  FPAF_A.Free;
  FPAF_B.Free;
  FPAF_C.Free;
  FPAF_D.Free;
  FPAF_E.Free;
  FPAF_F.Free;
  FPAF_G.Free;
  FPAF_H.Free;
  FPAF_J.Free;
  FPAF_L.Free;
  FPAF_M.Free;
  FPAF_N.Free;
  FPAF_P.Free;
  FPAF_R.Free;
  FPAF_S.Free;
  FPAF_T.Free;
  FPAF_TITP.Free;
  FPAF_W.Free;
  FPAF_U.Free;
  FPAF_V.Free;
  FPAF_Z.Free;

  if Assigned( fsEADInterno ) then
     FreeAndNil( fsEADInterno );

  inherited;
end;

function TACBrPAF.GetPath: String;
begin
  if FPath = '' then
    if not (csDesigning in ComponentState) then
      FPath := ExtractFilePath( ParamStr(0) );

  Result := FPath;
end;

function TACBrPAF.GetTrimString: boolean;
begin
  Result := FTrimString;
end;

procedure TACBrPAF.SetLinesBuffer(AValue: Integer);
begin
  if FLinesBuffer = AValue then Exit;
  FLinesBuffer := max(AValue,0);

  FPAF_A.LinhasBuffer    := AValue;
  FPAF_B.LinhasBuffer    := AValue;
  FPAF_C.LinhasBuffer    := AValue;
  FPAF_D.LinhasBuffer    := AValue;
  FPAF_E.LinhasBuffer    := AValue;
  FPAF_F.LinhasBuffer    := AValue;
  FPAF_G.LinhasBuffer    := AValue;
  FPAF_H.LinhasBuffer    := AValue;
  FPAF_J.LinhasBuffer    := AValue;
  FPAF_L.LinhasBuffer    := AValue;
  FPAF_M.LinhasBuffer    := AValue;
  FPAF_N.LinhasBuffer    := AValue;
  FPAF_P.LinhasBuffer    := AValue;
  FPAF_R.LinhasBuffer    := AValue;
  FPAF_S.LinhasBuffer    := AValue;
  FPAF_T.LinhasBuffer    := AValue;
  FPAF_TITP.LinhasBuffer := AValue;
  FPAF_W.LinhasBuffer    := AValue;
  FPAF_U.LinhasBuffer    := AValue;
  FPAF_V.LinhasBuffer    := AValue;
  FPAF_Z.LinhasBuffer    := AValue;
end;

procedure TACBrPAF.SetPath(const AValue: String);
begin
  FPath := PathWithDelim(Trim(AValue));
end;

procedure TACBrPAF.SetTrimString(const Value: boolean);
begin
  FTrimString := Value;

  FPAF_A.TrimString := Value;
  FPAF_B.TrimString := Value;
  FPAF_C.TrimString := Value;
  FPAF_D.TrimString := Value;
  FPAF_E.TrimString := Value;
  FPAF_F.TrimString := Value;
  FPAF_G.TrimString := Value;
  FPAF_H.TrimString := Value;
  FPAF_J.TrimString := Value;
  FPAF_L.TrimString := Value;
  FPAF_M.TrimString := Value;
  FPAF_N.TrimString := Value;
  FPAF_P.TrimString := Value;
  FPAF_R.TrimString := Value;
  FPAF_S.TrimString := Value;
  FPAF_T.TrimString := Value;
  FPAF_TITP.TrimString := Value;
  FPAF_W.TrimString := Value;
  FPAF_U.TrimString := Value;
  FPAF_V.TrimString := Value;
  FPAF_Z.TrimString := Value;
end;

function TACBrPAF.AjustaNomeArquivo(Arquivo: String): String;
var
  APath: String;
begin
  Arquivo := Trim(Arquivo);
  if EstaVazio(Arquivo) then
    raise Exception.Create('Caminho ou nome do arquivo não informado!');

  APath := ExtractFilePath(Arquivo);
  if EstaVazio(APath) then
  begin
    APath := Path;
    Arquivo := APath + Arquivo;
  end;

  if not DirectoryExists(APath) then
  begin
    ForceDirectories(APath);

    if not DirectoryExists(APath) then
      raise Exception.Create('Caminho do arquivo não existe!');
  end;

  Result := Arquivo;
end;

function TACBrPAF.GetOnError: TErrorEvent;
begin
  Result := FOnError;
end;

procedure TACBrPAF.SetOnError(const Value: TErrorEvent);
begin
  FOnError := Value;

  FPAF_A.OnError := Value;
  FPAF_B.OnError := Value;
  FPAF_C.OnError := Value;
  FPAF_D.OnError := Value;
  FPAF_E.OnError := Value;
  FPAF_F.OnError := Value;
  FPAF_G.OnError := Value;
  FPAF_H.OnError := Value;
  FPAF_J.OnError := Value;
  FPAF_L.OnError := Value;
  FPAF_M.OnError := Value;
  FPAF_N.OnError := Value;
  FPAF_P.OnError := Value;
  FPAF_R.OnError := Value;
  FPAF_S.OnError := Value;
  FPAF_T.OnError := Value;
  FPAF_TITP.OnError := Value;
  FPAF_W.OnError := Value;
  FPAF_U.OnError := Value;
  FPAF_V.OnError := Value;
  FPAF_Z.OnError := Value;
end;

procedure TACBrPAF.SetEAD(const AValue : TACBrEAD) ;
begin
  if AValue <> fsEAD then
  begin
     if Assigned(fsEAD) then
        fsEAD.RemoveFreeNotification(Self);

     fsEAD := AValue;

     if AValue <> nil then
     begin
        AValue.FreeNotification(self);

        if Assigned( fsEADInterno ) then
           FreeAndNil( fsEADInterno );
     end ;
  end ;
end ;

procedure TACBrPAF.SetAAC(const AValue : TACBrAAC) ;
begin
  if AValue <> fsAAC then
  begin
     if Assigned(fsAAC) then
        fsAAC.RemoveFreeNotification( Self );

     fsAAC := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end ;

function TACBrPAF.SaveToFile_TITP(Arquivo: String): Boolean;
begin
//  Result := False;
  Arquivo := AjustaNomeArquivo(Arquivo);

  SysUtils.DeleteFile(Arquivo);

  with FPAF_TITP do
  begin
    Conteudo.Clear;
    NomeArquivo := Arquivo;

    if Trim(Titulo) <> '' then
    begin
      Add(Titulo);
      Add(LinhaSimples(116));
    end;

    WriteMercadorias;

    Add(LinhaSimples(116));

    if DataHora < 0 then
      DataHora := NOW;

    Add(Format('Arquivo gerado em: %s %52d mercadoria(s) listada(s)', [
      FormatDateTime('dd/mm/yyyy "as" hh:mm', DataHora),
      Mercadorias.Count]));

    WriteBuffer;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;

  Result := True;
end;

function TACBrPAF.SaveToFile_W(Arquivo: String): Boolean;
begin
//  Result := False;
  Arquivo := AjustaNomeArquivo(Arquivo);

  SysUtils.DeleteFile(Arquivo);

  with PAF_W do
  begin
    Conteudo.Clear;
    NomeArquivo := Arquivo;

    WriteRegistroW1;
    WriteBuffer;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;

  Result:= True;
end;

function TACBrPAF.SaveToFile_Z(Arquivo: String): Boolean;
begin
//  Result := False;
  Arquivo := AjustaNomeArquivo(Arquivo);

  SysUtils.DeleteFile(Arquivo);

  with PAF_Z do
  begin
    Conteudo.Clear;
    NomeArquivo := Arquivo;

    WriteRegistroZ1;
    WriteBuffer;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;

  Result:= True;
end;

function TACBrPAF.SaveToFile_V(Arquivo: String): Boolean;
begin
  Arquivo := AjustaNomeArquivo(Arquivo);

  SysUtils.DeleteFile(Arquivo);

  with PAF_V do
  begin
    Conteudo.Clear;
    NomeArquivo := Arquivo;

    WriteRegistroV1;
    WriteBuffer;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(Arquivo);

    // Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;

  Result:= True;
end;

function TACBrPAF.SaveToFile_N(Arquivo: String): Boolean;
var
  PAF_MD5 : String ;
  iFor: Integer;
begin
//  Result := False;
  Arquivo := AjustaNomeArquivo(Arquivo);

  if Assigned( fsAAC ) then
  begin
     // Copie do AAC campos não informados do N1 //
    with FPAF_N.RegistroN1 do
    begin
      CNPJ        := ifthen( CNPJ = '', fsAAC.IdentPAF.Empresa.CNPJ, CNPJ ) ;
      IE          := ifthen( IE = '', fsAAC.IdentPAF.Empresa.IE, IE ) ;
      IM          := ifthen( IM = '', fsAAC.IdentPAF.Empresa.IM, IM ) ;
      RAZAOSOCIAL := ifthen( RAZAOSOCIAL = '', fsAAC.IdentPAF.Empresa.RazaoSocial,
                             RAZAOSOCIAL ) ;
    end;

    // Copie do AAC campos não informados do N2 //
    with FPAF_N.RegistroN2 do
    begin
      LAUDO  := ifthen( LAUDO = '', fsAAC.IdentPAF.NumeroLaudo, LAUDO ) ;
      NOME   := ifthen( NOME = '', fsAAC.IdentPAF.paf.Nome, NOME ) ;
      VERSAO := ifthen( VERSAO = '', fsAAC.IdentPAF.Paf.Versao, VERSAO ) ;
    end;

    // Se informou os arquivos no ACBrAAC copie-os para o N3 //
    if fsAAC.IdentPAF.OutrosArquivos.Count > 0 then
    begin
      FPAF_N.RegistroN3.Clear;
      For iFor := 0 to fsAAC.IdentPAF.OutrosArquivos.Count-1 do
      begin
        with FPAF_N.RegistroN3.New do
        begin
          NOME_ARQUIVO := fsAAC.IdentPAF.OutrosArquivos[iFor].Nome;
	        MD5          := Trim(fsAAC.IdentPAF.OutrosArquivos[iFor].MD5);
        end ;
      end;
    end;
  end ;

  SysUtils.DeleteFile(Arquivo);

  with FPAF_N do
  begin
    Conteudo.Clear;
    NomeArquivo := Arquivo;

    // Gravando arquivo N //
    WriteRegistroN1;
    WriteRegistroN2;

    if RegistroN3.Count > 0 then
      WriteRegistroN3;

    WriteRegistroN9;
    WriteBuffer;

    // Assinatura EAD
    if FAssinar then
      AssinaArquivoComEAD(Arquivo);

    // Sincronizando arquivos e MD5 do ACBrPAF com ACBrAAC //
    if Assigned( fsAAC ) then
    begin
      AAC.IdentPAF.OutrosArquivos.Clear ;

      // Alimenta a lista de arquivos autenticados no AAC, para que essa lista
      // possa ser usada na impressão do relatório "Identificação do PAF-ECF"
      for iFor := 0 to RegistroN3.Count - 1 do
      begin
        with AAC.IdentPAF.OutrosArquivos.New do
        begin
          Nome := RegistroN3.Items[iFor].NOME_ARQUIVO;
          MD5  := RegistroN3.Items[iFor].MD5;
        end;
      end;

      // Gera o MD5 do arquivo
      PAF_MD5 := GetACBrEAD.MD5FromFile( Arquivo );

      // Informações do arquivo com a lista de arquivos autenticados
      AAC.IdentPAF.ArquivoListaAutenticados.Nome := ExtractFileName(Arquivo);
      // Atualiza AAC.IdentPAF.ArquivoListaAutenticados.MD5
      AAC.AtualizarMD5( PAF_MD5 );
    end ;

    // Limpa de todos os Blocos as listas de todos os registros.
    LimpaRegistros;
  end;

  Result:= True;
end;

procedure TACBrPAF.Notification(AComponent : TComponent ; Operation : TOperation
   ) ;
begin
   inherited Notification(AComponent, Operation) ;

  if (Operation = opRemove) and (AComponent is TACBrEAD) and (fsEAD <> nil) then
     fsEAD := nil ;

  if (Operation = opRemove) and (AComponent is TACBrAAC) and (fsAAC <> nil) then
     fsAAC := nil ;
end ;

function TACBrPAF.GetACBrEAD : TACBrEAD ;
begin
  if Assigned(fsEAD) then
     Result := fsEAD
  else
   begin
     if not Assigned( fsEADInterno ) then
     begin
        fsEADInterno := TACBrEAD.Create(Self);
        fsEADInterno.OnGetChavePrivada := fsOnPAFGetKeyRSA;
     end ;
     Result := fsEADInterno;
   end ;
end ;

function TACBrPAF.AssinaArquivoComEAD(const Arquivo: String): Boolean;
begin
  if Assigned( fsOnPAFCalcEAD ) then
     fsOnPAFCalcEAD( Arquivo )
  else
     GetACBrEAD.AssinarArquivoComEAD( Arquivo ) ;

  Result := True;
end;

function TACBrPAF.SaveToFile_RegistrosPAF(Arquivo: String): Boolean;
begin
//  Result := False;
  Arquivo := AjustaNomeArquivo(Arquivo);

  SysUtils.DeleteFile(Arquivo);

  FPAF_U.Conteudo.Clear;
  FPAF_A.Conteudo.Clear;
  FPAF_P.Conteudo.Clear;
  FPAF_E.Conteudo.Clear;
  FPAF_D.Conteudo.Clear;
  FPAF_B.Conteudo.Clear;
  FPAF_C.Conteudo.Clear;
  FPAF_F.Conteudo.Clear;
  FPAF_T.Conteudo.Clear;
  FPAF_M.Conteudo.Clear;
  FPAF_L.Conteudo.Clear;
  FPAF_G.Conteudo.Clear;
  FPAF_H.Conteudo.Clear;
  FPAF_S.Conteudo.Clear;
  FPAF_R.Conteudo.Clear;
  FPAF_V.Conteudo.Clear;
  FPAF_Z.Conteudo.Clear;
  FPAF_J.Conteudo.Clear;

  FPAF_U.NomeArquivo := Arquivo;
  FPAF_A.NomeArquivo := Arquivo;
  FPAF_P.NomeArquivo := Arquivo;
  FPAF_E.NomeArquivo := Arquivo;
  FPAF_D.NomeArquivo := Arquivo;
  FPAF_B.NomeArquivo := Arquivo;
  FPAF_C.NomeArquivo := Arquivo;
  FPAF_F.NomeArquivo := Arquivo;
  FPAF_T.NomeArquivo := Arquivo;
  FPAF_M.NomeArquivo := Arquivo;
  FPAF_L.NomeArquivo := Arquivo;
  FPAF_G.NomeArquivo := Arquivo;
  FPAF_H.NomeArquivo := Arquivo;
  FPAF_S.NomeArquivo := Arquivo;
  FPAF_R.NomeArquivo := Arquivo;
  FPAF_V.NomeArquivo := Arquivo;
  FPAF_Z.NomeArquivo := Arquivo;
  FPAF_J.NomeArquivo := Arquivo;

  PAF_U.WriteRegistroU1;
  PAF_U.WriteBuffer;

  if FPAF_A.RegistroA2.Count > 0 then
  begin
    FPAF_A.WriteRegistroA2;
    FPAF_A.WriteBuffer;
  end;

  if FPAF_P.RegistroP2.Count > 0 then
  begin
    FPAF_P.WriteRegistroP2;
    FPAF_P.WriteBuffer;
  end;

  if FPAF_E.RegistroE2.Count > 0 then
  begin
    FPAF_E.WriteRegistroE2;
  end;

  if FPAF_E.RegistroE3.DT_EST > 0 then
  begin
    FPAF_E.WriteRegistroE3;
  	FPAF_E.WriteBuffer;
  end;

  if FPAF_D.RegistroD2.Count > 0 then
  begin
    FPAF_D.WriteRegistroD2;  //D2 e D3 e D4
    FPAF_D.WriteBuffer;
  end;

  if FPAF_B.RegistroB2.Count > 0 then
  begin
    FPAF_B.WriteRegistroB2;
    FPAF_B.WriteBuffer;
  end;

  if FPAF_C.RegistroC2.Count > 0 then
  begin
    FPAF_C.WriteRegistroC2;
    FPAF_C.WriteBuffer;
  end;

  if FPAF_F.RegistroF2.Count > 0 then
    FPAF_F.WriteRegistroF2;

  if FPAF_F.RegistroF3.Count > 0 then
    FPAF_F.WriteRegistroF3;

  if FPAF_F.RegistroF4.Count > 0 then
    FPAF_F.WriteRegistroF4;

  FPAF_F.WriteBuffer;

  if FPAF_T.RegistroT2.Count > 0 then
  begin
    FPAF_T.WriteRegistroT2;
    FPAF_T.WriteBuffer;
  end;

  if FPAF_M.RegistroM2.Count > 0 then
  begin
    FPAF_M.WriteRegistroM2;
    FPAF_M.WriteBuffer;
  end;

  if FPAF_L.RegistroL2.Count > 0 then
  begin
    FPAF_L.WriteRegistroL2;
    FPAF_L.WriteBuffer;
  end;

  if FPAF_G.RegistroG2.Count > 0 then
  begin
    FPAF_G.WriteRegistroG2;
    FPAF_G.WriteBuffer;
  end;

  if FPAF_H.RegistroH2.Count > 0 then
  begin
    FPAF_H.WriteRegistroH2;
    FPAF_H.WriteBuffer;
  end;

  if FPAF_S.RegistroS2.Count > 0 then        //S2 e S3
  begin
    FPAF_S.WriteRegistroS2;
    FPAF_S.WriteBuffer;
  end;

  FPAF_R.WriteRegistroR01;
  FPAF_R.WriteBuffer;

  if FPAF_J.RegistroJ1.Count > 0 then
  begin
    FPAF_J.WriteRegistroJ1;  //J1 e J2
    FPAF_J.WriteBuffer;
  end;

  // Assinatura EAD
  if FAssinar then
    AssinaArquivoComEAD(Arquivo);

  //Limpa de todos os Blocos as listas de todos os registros.
  FPAF_U.LimpaRegistros;
  FPAF_A.LimpaRegistros;
  FPAF_P.LimpaRegistros;
  FPAF_E.LimpaRegistros;
  FPAF_D.LimpaRegistros;
  FPAF_B.LimpaRegistros;
  FPAF_C.LimpaRegistros;
  FPAF_F.LimpaRegistros;
  FPAF_T.LimpaRegistros;
  FPAF_M.LimpaRegistros;
  FPAF_L.LimpaRegistros;
  FPAF_G.LimpaRegistros;
  FPAF_H.LimpaRegistros;
  FPAF_S.LimpaRegistros;
  FPAF_R.LimpaRegistros;
  FPAF_V.LimpaRegistros;
  FPAF_Z.LimpaRegistros;
  FPAF_J.LimpaRegistros;

  Result := True;
end;

{$IFNDEF NOGUI}
{$IFDEF FPC}
initialization
   {$I ACBrPAF.lrs}
{$ENDIF}
{$ENDIF}

end.
