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

unit ACBrConsultaCNPJ;

interface

uses
  SysUtils, Classes, types, IniFiles,
  ACBrBase, ACBrSocket, ACBrIBGE, ACBrConsultaCNPJ.WS;

type
  TACBrOnSolicitaCaptchaHTTP = procedure( var AHtml : String ) of object ;
  EACBrConsultaCNPJException = class ( Exception );
  TACBrCNPJProvedorWS = (cwsNenhum, cwsBrasilAPI, cwsReceitaWS, cwsPublica);

  { TACBrConsultaCNPJ }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrConsultaCNPJ = class(TACBrHTTP)
  protected
    FACBrIBGE: TACBrIBGE;
    FNaturezaJuridica : String ;
    //FViewState: String;
    FEmpresaTipo: String;
    FAbertura: TDateTime;
    FRazaoSocial: String;
    FFantasia: String;
    FPorte: String;
    FCNAE1: String;
    FCNAE2: TStringList;
    FEndereco: String;
    FNumero: String;
    FComplemento: String;
    FCEP: String;
    FBairro: String;
    FCidade: String;
    FUF: String;
    FSituacao: String;
    FSituacaoEspecial : String;
    FCNPJ: String;
    FDataSituacao: TDateTime;
    FDataSituacaoEspecial : TDateTime;
    FEndEletronico: String;
    FTelefone: String;
    FEFR: string;  //ENTE FEDERATIVO RESPONSÁVEL (EFR)
    FMotivoSituacaoCad: string;
    FPesquisarIBGE: Boolean;
    FCodigoIBGE: String;
    FIniServicos: string;
    FResourceName: String;
    FParams: TStrings;
    FOnSolicitarCaptcha: TACBrOnSolicitaCaptchaHTTP;
    FProvedor : TACBrCNPJProvedorWS;
    FUsuario: String;
    FSenha: String;
    FInscricaoEstadual : String;
    FDefasagemMaximo : Integer;
    //Function GetCaptchaURL: String;
    function GetIBGE_UF : String ;
    function VerificarErros(const Str: String): String;
    function LerCampo(Texto: TStringList; NomeCampo: String): String;
    function GetIniServicos: String;
    procedure LerParams;
    function LerSessaoChaveIni(const Sessao, Chave : String):String;
    function LerParamsIniServicos: AnsiString;
    function LerParamsInterno: AnsiString;
    procedure ParserWS(const AACBrConsultaCNPJWSResposta : TACBrConsultaCNPJWSResposta);
  public
    procedure Captcha(Stream: TStream); deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS}'Metodo sem utilidade atualmente.'{$ENDIF};
    function Consulta(const ACNPJ: String; ACaptcha: String = ''; ARemoverEspacosDuplos: Boolean = False): Boolean;
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnSolicitarCaptcha: TACBrOnSolicitaCaptchaHTTP read FOnSolicitarCaptcha write FOnSolicitarCaptcha;

    property CNPJ: String Read FCNPJ Write FCNPJ;
    property EmpresaTipo: String Read FEmpresaTipo;
    property Abertura: TDateTime Read FAbertura;
    property RazaoSocial: String Read FRazaoSocial;
    property Fantasia: String Read FFantasia;
    property Porte: String read FPorte;
    property CNAE1: String Read FCNAE1;
    property CNAE2: TStringList Read FCNAE2;
    property Endereco: String Read FEndereco;
    property Numero: String Read FNumero;
    property Complemento: String Read FComplemento;
    property CEP: String Read FCEP;
    property Bairro: String Read FBairro;
    property Cidade: String Read FCidade;
    property UF: String Read FUF;
    property Situacao: String Read FSituacao;
    property SituacaoEspecial: String Read FSituacaoEspecial;
    property DataSituacao: TDateTime Read FDataSituacao;
    property DataSituacaoEspecial : TDatetime Read FDataSituacaoEspecial;
    property NaturezaJuridica: String Read FNaturezaJuridica;
    property EndEletronico: string read FEndEletronico;
    property Telefone: String read FTelefone;
    property EFR: string read FEFR;
    property MotivoSituacaoCad: string read FMotivoSituacaoCad;
    property IBGE_Municipio  : String read FCodigoIBGE;
    property IBGE_UF         : String read GetIBGE_UF ;
    property PesquisarIBGE: Boolean read FPesquisarIBGE write FPesquisarIBGE;
    property IniServicos : string read GetIniServicos write FIniServicos;
    property Provedor : TACBrCNPJProvedorWS read FProvedor write FProvedor default cwsNenhum;
    property Usuario: String read FUsuario write FUsuario;
    property Senha: String read FSenha write FSenha;
    property InscricaoEstadual: String read FInscricaoEstadual;
    property DefasagemMaximo: Integer read FDefasagemMaximo write FDefasagemMaximo default 999;
  end;

implementation

uses
  strutils,
  blcksock, synautil,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.XMLHTML,
  ACBrValidador,
  ACBrUtil.FilesIO,
  ACBrConsultaCNPJ.WS.ReceitaWS,
  ACBrConsultaCNPJ.WS.BrasilAPI,
  ACBrConsultaCNPJ.WS.Publica;

{$IFDEF FPC}
 {$R ACBrConsultaCNPJServicos.rc}
{$ELSE}
 {$R ACBrConsultaCNPJServicos.res}
{$ENDIF}
(*function TACBrConsultaCNPJ.GetCaptchaURL : String ;
var
  AURL, Html: String;
begin
  try
    Self.HTTPGet('https://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/Cnpjreva_solicitacao3.asp');
    Html := Self.RespHTTP.Text;

    AURL := RetornarConteudoEntre(Html, '<img id="imgCaptcha" src="', '"');

    FViewState := RetornarConteudoEntre(Html, '<input type=hidden id=viewstate name=viewstate value='+'''', '''');

    Result := StringReplace(AURL, 'amp;', '', []);
  except
    on E: Exception do
    begin
      raise EACBrConsultaCNPJException.Create('Erro na hora de obter a URL do captcha.'+#13#10+E.Message);
    end;
  end;
end;*)

procedure TACBrConsultaCNPJ.Captcha(Stream: TStream);
var
  LErro : String;
begin
  if Self.Provedor = cwsNenhum then
   raise EACBrConsultaCNPJException.Create('Utilize comunicação via WebServices.');
  {try
    HTTPGet(LerSessaoChaveIni('ENDERECOS','CAPTCH'));  // GetCaptchaURL
    if HttpSend.ResultCode = 200 then
    begin
      HTTPSend.Document.Position := 0;
      Stream.CopyFrom(HttpSend.Document, HttpSend.Document.Size);
      Stream.Position := 0;
    end;
  except
    on E: Exception do
    begin
      LErro := 'Erro na hora de fazer o download da imagem do captcha.';
      if HttpSend.ResultCode = 404 then
        LErro := LErro + sLineBreak + 'Serviço depreciado/descontinuado pela Receita Federal do Brasil! não disponivel para consulta.'
      else
        LErro := LErro + sLineBreak + E.Message;

      raise EACBrConsultaCNPJException.Create(LErro);
    end;
  end;}
end;

function TACBrConsultaCNPJ.VerificarErros(const Str: String): String;
  var
    Res: String;
begin
  Res := '';
  if Res = '' then
    if Pos( ACBrStr('Imagem com os caracteres anti robô'), Str) > 0 then
      Res := 'Catpcha errado.';

  if Res = '' then
    if Pos( 'Erro na Consulta', Str ) > 0  then
       Res := 'Erro na Consulta. Atualize o Captcha';

  if Res = '' then
    if Pos(ACBrStr('Não existe no Cadastro de Pessoas Jurídicas o número de CNPJ informado. '+
                   'Verifique se o mesmo foi digitado corretamente.'), Str) > 0 then
      Res := 'Não existe no Cadastro de Pessoas Jurídicas o número de CNPJ informado. '+
             'Verifique se o mesmo foi digitado corretamente.';

  if Res = '' then
    if Pos(ACBrStr('a. No momento não podemos atender a sua solicitação. Por favor tente mais tarde.'), Str) > 0 then
      Res := 'Erro no site da receita federal. Tente mais tarde.';

  Result := ACBrStr(Res);
end;

function TACBrConsultaCNPJ.LerCampo(Texto: TStringList; NomeCampo: String
  ): String;
var
  i : integer;
  linha: String;
begin
  Result := '';
  for i := 0 to Texto.Count-1 do
  begin
    linha := Trim(Texto[i]);
    if linha = NomeCampo then
    begin
      Result := StringReplace(Trim(Texto[i+1]),'&nbsp;',' ',[rfReplaceAll]);
      Texto.Delete(I);
      break;
    end;
  end
end;

procedure TACBrConsultaCNPJ.LerParams;
var
  ConteudoParams: AnsiString;
begin
  ConteudoParams := LerParamsIniServicos;

  if ConteudoParams = '' then
    ConteudoParams := LerParamsInterno;

  FParams.Text := ConteudoParams;
end;

function TACBrConsultaCNPJ.LerParamsIniServicos: AnsiString;
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

function TACBrConsultaCNPJ.LerParamsInterno: AnsiString;
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

function TACBrConsultaCNPJ.LerSessaoChaveIni(const Sessao,
  Chave: String): String;
begin
  Result := FParams.Values[Chave];
end;

procedure TACBrConsultaCNPJ.ParserWS(const AACBrConsultaCNPJWSResposta : TACBrConsultaCNPJWSResposta);
begin
  FNaturezaJuridica     := AACBrConsultaCNPJWSResposta.NaturezaJuridica;
  FEmpresaTipo          := AACBrConsultaCNPJWSResposta.EmpresaTipo;
  FAbertura             := AACBrConsultaCNPJWSResposta.Abertura;
  FRazaoSocial          := AACBrConsultaCNPJWSResposta.RazaoSocial;
  FFantasia             := AACBrConsultaCNPJWSResposta.Fantasia;
  FPorte                := AACBrConsultaCNPJWSResposta.Porte;
  FCNAE1                := AACBrConsultaCNPJWSResposta.CNAE1;
  FCNAE2.Text           := AACBrConsultaCNPJWSResposta.CNAE2.Text;
  FEndereco             := AACBrConsultaCNPJWSResposta.Endereco;
  FNumero               := AACBrConsultaCNPJWSResposta.Numero;
  FComplemento          := AACBrConsultaCNPJWSResposta.Complemento;
  FCEP                  := AACBrConsultaCNPJWSResposta.CEP;
  FBairro               := AACBrConsultaCNPJWSResposta.Bairro;
  FCidade               := AACBrConsultaCNPJWSResposta.Cidade;
  FUF                   := AACBrConsultaCNPJWSResposta.UF;
  FSituacao             := AACBrConsultaCNPJWSResposta.Situacao;
  FSituacaoEspecial     := AACBrConsultaCNPJWSResposta.SituacaoEspecial;
  FCNPJ                 := AACBrConsultaCNPJWSResposta.CNPJ;
  FDataSituacao         := AACBrConsultaCNPJWSResposta.DataSituacao;
  FDataSituacaoEspecial := AACBrConsultaCNPJWSResposta.DataSituacaoEspecial;
  FEndEletronico        := AACBrConsultaCNPJWSResposta.EndEletronico;
  FTelefone             := AACBrConsultaCNPJWSResposta.Telefone;
  FEFR                  := AACBrConsultaCNPJWSResposta.EFR;
  FMotivoSituacaoCad    := AACBrConsultaCNPJWSResposta.MotivoSituacaoCad;
  FCodigoIBGE           := AACBrConsultaCNPJWSResposta.CodigoIBGE;
  FInscricaoEstadual    := AACBrConsultaCNPJWSResposta.InscricaoEstadual;
end;

function TACBrConsultaCNPJ.Consulta(const ACNPJ: String; ACaptcha: String; ARemoverEspacosDuplos: Boolean): Boolean;
var
  Html, Erro, StrAux, PostStr:String;
  Resposta : TStringList;
  CountCid, Tentativas:Integer;
  Retentar: Boolean;
  ModoAntigo: Boolean;

  LACBrConsultaCNPJWS : TACBrConsultaCNPJWS;
begin
  Erro := ValidarCNPJ( ACNPJ ) ;
  if Erro <> '' then
     raise EACBrConsultaCNPJException.Create(Erro);

  try
    if Self.Provedor <> cwsNenhum then
    begin
      case Self.Provedor of
        cwsReceitaWS : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSReceitaWS.Create( ACNPJ, Self.Usuario, Self.Senha, Self.DefasagemMaximo );
        cwsBrasilAPI : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSBrasilAPI.Create( ACNPJ, Self.Usuario, Self.Senha );
        cwsPublica   : LACBrConsultaCNPJWS := TACBrConsultaCNPJWSPublica.Create( ACNPJ, Self.Usuario, Self.Senha );
      end;

      Result := LACBrConsultaCNPJWS.Executar;
      ParserWS(LACBrConsultaCNPJWS.FResposta);
      Exit;
    end;
  finally
    LACBrConsultaCNPJWS.Free;
  end;

  Clear;
  Retentar := True;
  Tentativas := 0;
  ModoAntigo := True;

  if Assigned(FOnSolicitarCaptcha) then
  begin
    FOnSolicitarCaptcha(Html);

    RespHTTP.Text:= Html;

    ModoAntigo := False;
  end;

  if ModoAntigo then
  begin
    while Retentar do
    begin
      HTTPSend.Clear;
      PostStr := 'origem=comprovante&' +
                 'cnpj='+OnlyNumber(ACNPJ)+'&' +
                 'txtTexto_captcha_serpro_gov_br=' +Trim(ACaptcha) +'&' +
                 'search_type=cnpj';

      WriteStrToStream( HTTPSend.Document, PostStr );
      HTTPSend.MimeType := 'application/x-www-form-urlencoded';
      HTTPSend.Cookies.Add('flag=1');
      HTTPSend.Headers.Add('Referer: '+LerSessaoChaveIni('ENDERECOS','REFER'));
      HTTPPost(LerSessaoChaveIni('ENDERECOS','POST'));

      //DEBUG:
      //RespHTTP.SaveToFile('c:\temp\cnpj1.txt');

      Retentar := (Tentativas < 2) and
                  (pos('Captcha Sonoro', RespHTTP.Text) > 0) and
                  (pos(ACBrStr('Digite o número de CNPJ da empresa e clique em'), RespHTTP.Text) > 0);
      Inc( Tentativas );
    end;

    Erro := VerificarErros(RespHTTP.Text);

    if Erro <> '' then
    begin
      raise EACBrConsultaCNPJException.Create(Erro);
    end;
  end;

  Result:= True;
  Resposta := TStringList.Create;
  try
    Resposta.Text := StripHTML(RespHTTP.Text);
    RemoveEmptyLines( Resposta );

    //DEBUG:
    //Resposta.SaveToFile('c:\temp\cnpj2.txt');

    FCNPJ         := LerCampo(Resposta,ACBrStr('NÚMERO DE INSCRIÇÃO'));
    if FCNPJ <> '' then
      FEmpresaTipo  := LerCampo(Resposta,FCNPJ);
    FAbertura     := StringToDateTimeDef(LerCampo(Resposta,'DATA DE ABERTURA'),0);
    FRazaoSocial  := LerCampo(Resposta,'NOME EMPRESARIAL');
    FFantasia     := LerCampo(Resposta,ACBrStr('TÍTULO DO ESTABELECIMENTO (NOME DE FANTASIA)'));
    FPorte        := LerCampo(Resposta,'PORTE');
    FCNAE1        := LerCampo(Resposta,ACBrStr('CÓDIGO E DESCRIÇÃO DA ATIVIDADE ECONÔMICA PRINCIPAL'));
    FEndereco     := LerCampo(Resposta,'LOGRADOURO');
    FNumero       := LerCampo(Resposta,ACBrStr('NÚMERO'));
    FComplemento  := LerCampo(Resposta,'COMPLEMENTO');
    FCEP          := OnlyNumber( LerCampo(Resposta,'CEP') ) ;
    if FCEP <> '' then
      FCEP        := copy(FCEP,1,5)+'-'+copy(FCEP,6,3) ;

    FBairro       := LerCampo(Resposta,'BAIRRO/DISTRITO');
    FCidade       := LerCampo(Resposta,ACBrStr('MUNICÍPIO'));
    FUF           := LerCampo(Resposta,'UF');
    FSituacao     := LerCampo(Resposta,ACBrStr('SITUAÇÃO CADASTRAL'));
    FSituacaoEspecial     := LerCampo(Resposta,ACBrStr('SITUAÇÃO ESPECIAL'));
    FDataSituacao := StringToDateTimeDef(LerCampo(Resposta,ACBrStr('DATA DA SITUAÇÃO CADASTRAL')),0);
    FDataSituacaoEspecial := StringToDateTimeDef(LerCampo(Resposta,ACBrStr('DATA DA SITUAÇÃO ESPECIAL')),0);
    FNaturezaJuridica := LerCampo(Resposta,ACBrStr('CÓDIGO E DESCRIÇÃO DA NATUREZA JURÍDICA'));
    FEndEletronico:= LerCampo(Resposta, ACBrStr('ENDEREÇO ELETRÔNICO'));
    if Trim(FEndEletronico) = 'TELEFONE' then
      FEndEletronico := '';
    FTelefone     := LerCampo(Resposta, 'TELEFONE');
    FEFR          := LerCampo(Resposta, ACBrStr('ENTE FEDERATIVO RESPONSÁVEL (EFR)'));
    FMotivoSituacaoCad := LerCampo(Resposta, ACBrStr('MOTIVO DE SITUAÇÃO CADASTRAL'));

    FCNAE2.Clear;
    repeat
      StrAux := LerCampo(Resposta,ACBrStr('CÓDIGO E DESCRIÇÃO DAS ATIVIDADES ECONÔMICAS SECUNDÁRIAS'));
      if trim(StrAux) = '' then
        Break;

      FCNAE2.Add(RemoverEspacosDuplos(StrAux));

      repeat
        StrAux := LerCampo(Resposta, StrAux);
        if trim(StrAux) = '' then
          Break;

        FCNAE2.Add(RemoverEspacosDuplos(StrAux));
      until False;
    until False;
  finally
    Resposta.Free;
  end ;

  // Removendo astrísticos do inicio do nome da Cidade e UF..
  while (Length(FCidade) > 0) and (FCidade[1] = '*') do
    Delete(FCidade,1,1);

  while (Length(FUF) > 0) and (FUF[1] = '*') do
    Delete(FUF,1,1);

  // Consulta Codigo da Cidade ACBrIBGE
  fCodigoIBGE := '';
  if FPesquisarIBGE and (FCidade <> '') and (FUF <> '') then
  begin
    FACBrIBGE.BuscarPorNome( FCidade, FUF, False);

    if FACBrIBGE.Cidades.Count > 0 then  // Achou ?
    begin
      for CountCid := 0 to FACBrIBGE.Cidades.Count -1 do
      Begin
         if (UpperCase(TiraAcentos(FCidade)) = UpperCase(TiraAcentos(FACBrIBGE.Cidades[CountCid].Municipio))) And
            (FUF = FACBrIBGE.Cidades[CountCid].UF) then
         Begin
           FCodigoIBGE := IntToStr( FACBrIBGE.Cidades[CountCid].CodMunicipio );
           Break;
         End;
      end;
    end;
  end ;

  if Trim(FRazaoSocial) = '' then
    raise EACBrConsultaCNPJException.Create(ACBrStr('Não foi possível obter os dados.'));

  if ARemoverEspacosDuplos then
  begin
    FRazaoSocial := RemoverEspacosDuplos(FRazaoSocial);
    FFantasia    := RemoverEspacosDuplos(FFantasia);
    FEndereco    := RemoverEspacosDuplos(FEndereco);
    FNumero      := RemoverEspacosDuplos(FNumero);
    FComplemento := RemoverEspacosDuplos(FComplemento);
    FBairro      := RemoverEspacosDuplos(FBairro);
    FCidade      := RemoverEspacosDuplos(FCidade);
  end;
end;

constructor TACBrConsultaCNPJ.Create(AOwner: TComponent);
begin
  inherited;
  FCNAE2 := TStringList.Create;
  FPesquisarIBGE := False;
  fACBrIBGE := TACBrIBGE.Create(nil);
  FACBrIBGE.IgnorarCaixaEAcentos := True;
  HTTPSend.Sock.SSL.SSLType := LT_TLSv1;
  FResourceName := 'ACBrConsultaCNPJServicos';
  FParams := TStringList.Create;
  LerParams;
  FProvedor := cwsNenhum;
  FDefasagemMaximo := 999;
end;

destructor TACBrConsultaCNPJ.Destroy;
begin
  fACBrIBGE.Free;
  FCNAE2.Free;
  FParams.Free;
  inherited;
end;

procedure TACBrConsultaCNPJ.Clear;
begin
  FNaturezaJuridica := '';
  FEmpresaTipo      := '';
  FAbertura         := 0;
  FRazaoSocial      := '';
  FFantasia         := '';
  FPorte            := '';
  FCNAE1            := '';
  FEndereco         := '';
  FNumero           := '';
  FComplemento      := '';
  FCEP              := '';
  FBairro           := '';
  FCidade           := '';
  FUF               := '';
  FSituacao         := '';
  FSituacaoEspecial := '';
  FCNPJ             := '';
  FDataSituacao     := 0;
  FDataSituacaoEspecial     := 0;
  FEndEletronico    := '';
  FTelefone         := '';
  FEFR              := '';
  FMotivoSituacaoCad:= '';
  FCodigoIBGE       := '';
  FInscricaoEstadual:= '';

  FCNAE2.Clear;
end;

function TACBrConsultaCNPJ.GetIBGE_UF: String;
begin
  Result := copy(fCodigoIBGE,1,2) ;
end;

function TACBrConsultaCNPJ.GetIniServicos: String;
begin
  if FIniServicos = '' then
    FIniServicos := ApplicationPath + FResourceName +'.ini';
  Result := FIniServicos;
end;

end.
