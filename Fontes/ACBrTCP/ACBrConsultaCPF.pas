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

unit ACBrConsultaCPF;

interface

uses
  SysUtils, Classes, types, IniFiles,
  ACBrBase, ACBrSocket;

type
  EACBrConsultaCPFException = class ( Exception );

  { TACBrConsultaCPF }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrConsultaCPF = class(TACBrHTTP)
  private
    FDataNascimento: String;
    FDataInscricao: String;
    FNome: String;
    FNomeSocial : String;
    FSituacao: String;
    FCPF: String;
    FDigitoVerificador: String;
    FEmissao: String;
    FCodCtrlControle: String;
    FTokenCaptcha: String;
    FIniServicos: string;
    FResourceName: String;
    FParams: TStrings;

    function VerificarErros(const Str: String): String;
    function LerCampo(Texto: TStringList; NomeCampo: String): String;
    function GetCaptchaURL : String ;
    function GetIniServicos: String;
    procedure LerParams;
    function LerSessaoChaveIni(const Sessao, Chave : String):String;
    function LerParamsIniServicos: AnsiString;
    function LerParamsInterno: AnsiString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Captcha(Stream: TStream);
    function Consulta(const ACPF, DataNasc, ACaptcha: String;
      ARemoverEspacosDuplos: Boolean = False): Boolean;
  published
    property CPF: String Read FCPF Write FCPF;
    property DataNascimento : String Read FDataNascimento write FDataNascimento;
    property DataInscricao : String Read FDataInscricao write FDataInscricao;
    property Nome: String Read FNome;
    property Situacao: String Read FSituacao;
    property DigitoVerificador: String Read FDigitoVerificador;
    property Emissao: String Read FEmissao;
    property CodCtrlControle: String Read FCodCtrlControle;
    property IniServicos : string read GetIniServicos write FIniServicos;

  end;

implementation

{$IFDEF FPC}
 {$R ACBrConsultaCPFServicos.rc}
{$ELSE}
 {$R ACBrConsultaCPFServicos.res}
{$ENDIF}

uses
  strutils,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrValidador,
  ACBrUtil.FilesIO,
  synacode, synautil, blcksock;

constructor TACBrConsultaCPF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HTTPSend.Sock.SSL.SSLType := LT_TLSv1;
  Self.IsUTF8 := False;
  FResourceName := 'ACBrConsultaCPFServicos';
  FParams := TStringList.Create;
  LerParams;
end;

destructor TACBrConsultaCPF.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TACBrConsultaCPF.GetCaptchaURL : String ;
var
  AURL, Html: String;
begin
  try
    Self.HTTPGet(LerSessaoChaveIni('ENDERECOS','CAPTCH'));
    Html := Self.RespHTTP.Text;
    //Debug
    //WriteToTXT('C:\TEMP\ACBrConsultaCPF-Captcha.TXT',Html);
    AURL := RetornarConteudoEntre(Html, 'src="data:image/png;base64,', '">');
    
    Result := StringReplace(AURL, 'amp;', '', []);
  except
    on E: Exception do
    begin
      raise EACBrConsultaCPFException.Create('Erro na hora de obter a URL do captcha.'+#13#10+E.Message);
    end;
  end;
end;

function TACBrConsultaCPF.GetIniServicos: String;
begin
  if FIniServicos = '' then
    FIniServicos := ApplicationPath + FResourceName +'.ini';
  Result := FIniServicos;
end;

procedure TACBrConsultaCPF.Captcha(Stream: TStream);
begin
  try
    Stream.Size := 0; // Trunca o Stream
    WriteStrToStream(Stream, DecodeBase64(GetCaptchaURL));
    Stream.Position:= 0;
  Except
    on E: Exception do begin
      raise EACBrConsultaCPFException.Create('Erro na hora de fazer o download da imagem do captcha.'+#13#10+E.Message);
    end;
  end;
end;

function TACBrConsultaCPF.VerificarErros(const Str: String): String;
var
  Res: String;
begin
  Res := '';
  if Res = '' then
    if Pos( ACBrStr('Os caracteres da imagem não foram preenchidos corretamente'), Str) > 0 then
      Res := 'Os caracteres da imagem não foram preenchidos corretamente.';

  if Res = '' then
    if Pos(ACBrStr('O número do CPF não é válido. Verifique se o mesmo foi digitado corretamente.'), Str) > 0 then
      Res := 'O número do CPF não é válido. Verifique se o mesmo foi digitado corretamente.';

  if Res = '' then
    if Pos(ACBrStr('Não existe no Cadastro de Pessoas Jurídicas o número de CPF informado. '+
                   'Verifique se o mesmo foi digitado corretamente.'), Str) > 0 then
      Res := 'Não existe no Cadastro de Pessoas Jurídicas o número de CPF informado. '+
             'Verifique se o mesmo foi digitado corretamente.';

  if Res = '' then
    if Pos(ACBrStr('a. No momento não podemos atender a sua solicitação. Por favor tente mais tarde.'), Str) > 0 then
      Res := 'Erro no site da receita federal. Tente mais tarde.';

  Result := ACBrStr(Res);
end;

function TACBrConsultaCPF.LerCampo(Texto : TStringList ; NomeCampo : String
  ) : String ;
var
  i : integer;
  linha : String;
begin
  NomeCampo := ACBrStr(NomeCampo);
  Result := '';
  for i := 0 to Texto.Count-1 do
  begin
    linha := Texto[i];
    if Pos(NomeCampo, linha) > 0 then
    begin
      Result := Trim(StringReplace(linha, NomeCampo, ' ',[rfReplaceAll]));
      break;
    end;
  end
end;

procedure TACBrConsultaCPF.LerParams;
var
  ConteudoParams: AnsiString;
begin
  ConteudoParams := LerParamsIniServicos;

  if ConteudoParams = '' then
    ConteudoParams := LerParamsInterno;

  FParams.Text := ConteudoParams;
end;

function TACBrConsultaCPF.LerParamsIniServicos: AnsiString;
var
  ArqIni: String;
  FS: TFileStream;
begin
  Result := '';
  ArqIni := Trim(IniServicos);
  if (ArqIni <> '') and FileExists(ArqIni) then
  begin
    FS := TFileStream.Create(ArqIni, fmOpenRead or fmShareDenyNone);
    try
      FS.Position := 0;
      Result := ReadStrFromStream(FS, FS.Size);
    finally
      FS.Free;
    end;
  end;
end;

function TACBrConsultaCPF.LerParamsInterno: AnsiString;
var
  RS: TResourceStream;
begin
  Result := '';

  RS := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  try
    RS.Position := 0;
    Result := ReadStrFromStream(RS, RS.Size);
  finally
    RS.Free;
  end;
end;

function TACBrConsultaCPF.LerSessaoChaveIni(const Sessao,
  Chave: String): String;
begin
  Result := FParams.Values[Chave];
end;

function TACBrConsultaCPF.Consulta(const ACPF, DataNasc,  ACaptcha: String;
  ARemoverEspacosDuplos: Boolean): Boolean;
var
  Post: TStringStream;
  Erro: String;
  Resposta : TStringList;
begin
  Erro := ValidarCPF( ACPF ) ;
  if Erro <> '' then
     raise EACBrConsultaCPFException.Create(Erro);

  //txtCPF=11122334410&txtToken_captcha_serpro_gov_br=299218104152138191166941752496584741018616278361624164&txtTexto_captcha_serpro_gov_br=ZCI8B9&Enviar=Consultar
  Post:= TStringStream.Create('');
  try
    {Post.WriteString('txtCPF='+OnlyNumber(ACPF)+'&');
    //Post.WriteString('tempTxtNascimento='+dataNasc+'&');
    Post.WriteString('txtToken_captcha_serpro_gov_br='+FTokenCaptcha+'&');
    Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    Post.WriteString('Enviar=Consultar');}

    Post.WriteString('TxtCPF='+ACPF+'&');
    Post.WriteString('txtDataNascimento='+datanasc+'&');
    Post.WriteString('txtToken_captcha_serpro_gov_br='+FTokenCaptcha+'&');
    Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    //Post.WriteString('txtTexto_captcha_serpro_gov_br='+Trim(ACaptcha)+'&');
    Post.WriteString('Enviar=Consultar');

    Post.Position:= 0;

    HttpSend.Clear;
    HttpSend.Document.Position:= 0;
    HttpSend.Document.CopyFrom(Post, Post.Size);
    HTTPSend.MimeType := 'application/x-www-form-urlencoded';
    HTTPPost(LerSessaoChaveIni('ENDERECOS','POST'));

    //Debug
    //RespHTTP.SaveToFile('C:\TEMP\ACBrConsultaCPF-1.TXT');

    Erro := VerificarErros(RespHTTP.Text);

    if Erro = '' then
    begin
      Result:= True;
      Resposta := TStringList.Create;
      try
        Resposta.Text := StripHTML(RespHTTP.Text);
        Resposta.Text := ACBrUtil.XMLHTML.ParseText( Resposta.Text );
        RemoveEmptyLines( Resposta );

        //Debug
        //Resposta.SaveToFile('C:\TEMP\ACBrConsultaCPF-2.TXT');

        FCPF      := LerCampo(Resposta,'No do CPF:');
        FNome     := LerCampo(Resposta,'Nome:');
        if FNome = '' then
          FNome   := LerCampo(Resposta,'Nome Civil:');

        FNomeSocial   := LerCampo(Resposta,'Nome Social:');

        FDataNascimento := LerCampo(Resposta,'Data de Nascimento:');
        FSituacao := LerCampo(Resposta,'Situação Cadastral:');
        FDataInscricao := LerCampo(Resposta,'Data da Inscrição:');
        FEmissao  := LerCampo(Resposta,'Comprovante emitido às:');
        FCodCtrlControle   := LerCampo(Resposta,'Código de controle do comprovante:');
        FDigitoVerificador := LerCampo(Resposta,'Digito Verificador:');

        if Trim(FNome) = '' then
        begin
          Erro     := LerCampo(Resposta,'Data de nascimento informada');
          if Trim(Erro) <> '' then
            Erro := 'Erro de data';
        end;
      finally
        Resposta.Free;
      end ;

      if Trim(Erro) = 'Erro de data' then
        raise EACBrConsultaCPFException.Create('Data de nascimento divergente da base da Receita Federal.');

      if Trim(FNome) = '' then
        raise EACBrConsultaCPFException.Create(ACBrStr('Não foi possível obter os dados.'));

      if ARemoverEspacosDuplos then
      begin
        FNome := RemoverEspacosDuplos(FNome);
      end;
    end
    else
    begin
      //Se está sendo levantada uma exception, não faz sentido ter retorno na função.
//      Result:= False;
      raise EACBrConsultaCPFException.Create(Erro);
    end;
  finally
    Post.Free;
  end;
end;

end.

