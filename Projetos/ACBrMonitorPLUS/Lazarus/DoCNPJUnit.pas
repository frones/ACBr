{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: José M S Junior                                  }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit DoCNPJUnit;

interface

uses
  Classes, SysUtils, ACBrConsultaCNPJ, ACBrMonitorConsts, ACBrMonitorConfig,
  ACBrLibResposta,  ACBrLibConsultaCNPJRespostas,
  CmdUnit, ACBrUtil.FilesIO, ACBrUtil.Strings;

Const
  CCAPTCHA_CNPJ = 'CaptchaCNPJ';

Type

{ TACBrObjetoConsultaCNPJ }

TACBrObjetoConsultaCNPJ = class(TACBrObjetoDFe)
private
  fACBrConsultaCNPJ: TACBrConsultaCNPJ;
public
  constructor Create(AConfig: TMonitorConfig; ACBrConsultaCNPJ: TACBrConsultaCNPJ); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaConsulta;
  function EncodeCaptchaBase64(AStream: TMemoryStream):AnsiString;
  procedure DecodeCaptchaBase64(const ABase64Captcha: Ansistring; AStream: TMemoryStream);

  property ACBrConsultaCNPJ: TACBrConsultaCNPJ read fACBrConsultaCNPJ;
end;

{ TMetodoConsultarCaptcha}
TMetodoConsultarCaptcha = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultar}
TMetodoConsultar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetProvedor  }
TMetodoSetProvedor  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

implementation

uses
  synautil, Graphics, synacode, pcnAuxiliar;

{ TMetodoConsultar }

{ Params: 0 - String: CNPJ para Consulta }
procedure TMetodoConsultar.Executar;
var
  ACNPJ: String;
begin
  ACNPJ := trim(fpCmd.Params(0));

  if (ACNPJ = '') then
    raise Exception.Create('CNPJ não informado.')
  else
  begin
    if not( ValidarCNPJouCPF(ACNPJ) ) then
      raise Exception.Create('CNPJ ' + ACNPJ + ' inválido.');
  end;

  with TACBrObjetoConsultaCNPJ(fpObjetoDono) do
  begin
    ACBrConsultaCNPJ.Consulta(ACNPJ);
    RespostaConsulta;
  end;

end;

{ TMetodoConsultarCaptcha }

{ Params: 0 - String: Path para gerar png Captcha}
procedure TMetodoConsultarCaptcha.Executar;
var
  AStream: TMemoryStream;
  APath: String;
begin
  APath := trim(fpCmd.Params(0));

  if (APath <> '') and not( DirectoryExists( PathWithDelim( APath ) )) then
      raise Exception.Create('Diretorio: ' + PathWithDelim( APath ) +' não encontrado.');

  AStream:= TMemoryStream.Create;
  try
    with TACBrObjetoConsultaCNPJ(fpObjetoDono) do
    begin
      ACBrConsultaCNPJ.Captcha(AStream);
      if (APath <> '') then
      begin
        try
          AStream.SaveToFile( PathWithDelim( APath ) + CCAPTCHA_CNPJ + '.png' );
          fpCmd.Resposta := PathWithDelim( APath ) + CCAPTCHA_CNPJ + '.png';
        except
          raise Exception.Create(ACBrStr('Falha ao gravar arquivo ' + CCAPTCHA_CNPJ + '.png'));
        end;
      end
      else
      begin
        fpCmd.Resposta :=  EncodeCaptchaBase64(AStream);

        //Decodifica
        {DecodeCaptchaBase64(fpCmd.Resposta, AStream2);
         AStream2.SaveToFile('c:\temp\captcha.png');}
      end;

    end;
  finally
    AStream.Free;

  end;
end;


{ TACBrObjetoConsultaCNPJ }

constructor TACBrObjetoConsultaCNPJ.Create(AConfig: TMonitorConfig;
  ACBrConsultaCNPJ: TACBrConsultaCNPJ);
begin
  inherited Create(AConfig);

  fACBrConsultaCNPJ := ACBrConsultaCNPJ;

  ListaDeMetodos.Add(CMetodoConsultarCaptcha);
  ListaDeMetodos.Add(CMetodoConsultar);
  ListaDeMetodos.Add(CMetodoSetProvedor);

end;

procedure TACBrObjetoConsultaCNPJ.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoConsultarCaptcha;
    1  : AMetodoClass := TMetodoConsultar;
    2  : AMetodoClass := TMetodoSetProvedor;

  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;

end;

procedure TACBrObjetoConsultaCNPJ.RespostaConsulta;
var
  Resp: TLibConsultaCNPJConsulta;
begin
  Resp := TLibConsultaCNPJConsulta.Create(TpResp, codUTF8);
  try
    with fACBrConsultaCNPJ do
    begin
      Resp.EmpresaTipo:= EmpresaTipo;
      Resp.RazaoSocial:= RazaoSocial;
      Resp.Abertura:= Abertura;
      Resp.Fantasia:= Fantasia;
      Resp.Endereco:= Endereco;
      Resp.Numero:= Numero;
      Resp.Complemento:= Complemento;
      Resp.Bairro:= Bairro;
      Resp.Cidade:= Cidade;
      Resp.UF:= UF;
      Resp.CEP:= CEP;
      Resp.Situacao:= Situacao;
      Resp.CNAE1:= CNAE1;
      Resp.CNAE2:= StringReplace(CNAE2.Text, #13#10,' | ',[rfReplaceAll]);
      Resp.NaturezaJuridica:= NaturezaJuridica;
      Resp.InscricaoEstadual:= InscricaoEstadual;
      fpCmd.Resposta := sLineBreak + Resp.Gerar;
    end;
  finally
    Resp.Free;
  end;

end;

function TACBrObjetoConsultaCNPJ.EncodeCaptchaBase64(AStream: TMemoryStream): AnsiString;
var
  ADataPng: AnsiString;
begin
  Result:= '';
  try
    AStream.Position:= 0;
    ADataPng := ReadStrFromStream(AStream, AStream.Size);
    Result := EncodeBase64(ADataPng);

  Except
    raise Exception.Create('Falha ao Converter imagem para base64 ');
  end;
end;

procedure TACBrObjetoConsultaCNPJ.DecodeCaptchaBase64(const ABase64Captcha: Ansistring; AStream: TMemoryStream);
var
  ADataPng: AnsiString;
begin
  try
     ADataPng := DecodeBase64(ABase64Captcha);
     AStream.Position:= 0;
     WriteStrToStream(AStream, ADataPng);

  Except
    raise Exception.Create('Falha ao Decodificar base64 da imagem ');
  end;

end;

{ TMetodoSetProvedor }

{ Params: 0 - AProvedor : Integer com Indice Provedor
          1 - AUsuario: String com Nome Usuario
          2 - ASenha: String com a Senha usuario
}
procedure TMetodoSetProvedor.Executar;
var
  AProvedor: Integer;
  AUsuario, Asenha: String;
begin
  AProvedor := StrToIntDef(fpCmd.Params(0),0);
  AUsuario  := fpCmd.Params(1);
  ASenha    := fpCmd.Params(2);

  with TACBrObjetoConsultaCNPJ(fpObjetoDono) do
  begin
    with MonitorConfig.ConsultaCNPJ do
    begin
      Provedor:= AProvedor;
      Usuario := AUsuario;
      Senha   := Asenha;
      MonitorConfig.SalvarArquivo;
    end;

  end;

end;




end.

