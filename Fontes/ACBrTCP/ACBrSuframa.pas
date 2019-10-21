{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 12/08/2010: Primeira Versao
|*    Régys Borges da Silveira
******************************************************************************}
unit ACBrSuframa;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, ACBrUtil, ACBrSocket, ACBrValidador;

type
  EACBrSuframa = class( Exception );

  TACBrSuframaSituacao = class
  private
    FCodigo: Integer;
    function GetDescricao: string;
  public
    constructor Create;
    procedure Clear;
  //published
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: string read GetDescricao;
  end;
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or
  pidiOSSimulator or  pidAndroid or
  pidLinux32 or pidiOSDevice
  {$IFDEF RTL300_UP}
  or pidiOSDevice32 or pidLinux64
  or pidWinNX32 or pidWinIoT32
  or pidiOSDevice64
  or pidOSX64 or pidLinux32Arm
  or pidLinux64Arm or pidAndroid64Arm
  {$ENDIF RTL300_UP})]
  {$ENDIF RTL230_UP}
  TACBrSuframa = class( TACBrHTTP )
  private
    fOnBuscaEfetuada: TNotifyEvent ;
    FSituacao: TACBrSuframaSituacao;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy ; override;
    procedure ConsultarSituacao(const ASuframa, ACnpj: AnsiString);
  published
    property Situacao: TACBrSuframaSituacao read FSituacao;
    property OnBuscaEfetuada: TNotifyEvent read fOnBuscaEfetuada write fOnBuscaEfetuada;
  end;

implementation

const
  URL_WEBSERVICE = 'https://servicos.suframa.gov.br/cadastroWS/services/CadastroWebService';

{ TACBrSuframaSituacao }

procedure TACBrSuframaSituacao.Clear;
begin
  FCodigo := 0;
end;

constructor TACBrSuframaSituacao.Create;
begin
  Self.Clear;
end;

function TACBrSuframaSituacao.GetDescricao: string;
begin
  case FCodigo of
    0: Result := 'Ocorreu erro na conexão com o webservice';
    1: Result := 'Empresa não habilitada';
    2: Result := 'Empresa habilitada';
    3: Result := 'Empresa não encontrada (CNPJ ou a Inscrição Suframa estão incorretos ou não existem no sistema)';
  else
    Result := 'Descrição da situação ainda não implementada';
  end;
end;

{ TACBrSuframa }

constructor TACBrSuframa.Create(AOwner: TComponent);
begin
  inherited;

  FSituacao := TACBrSuframaSituacao.Create;
  fOnBuscaEfetuada := nil;
  Self.ParseText := True;
end;

destructor TACBrSuframa.Destroy;
begin
  FSituacao.Free;
  inherited;
end;

procedure TACBrSuframa.ConsultarSituacao(const ASuframa, ACnpj: AnsiString);
var
  Acao: TStringList;
  ParametrosConsulta: String;
  Stream: TMemoryStream;
  ErroCodigo, ErroMsg: String;
  Retorno: String;
begin
  FSituacao.Clear;

  ErroMsg := ValidarSuframa( AnsiString( ACBrUtil.OnlyNumber( ASuframa ) ) );
  if ErroMsg <> '' then
    raise EACBrSuframa.Create( 'Erro de validação: ' + sLineBreak + String( ErroMsg ) );

  if ACnpj <> '' then
  begin
    ErroMsg := ValidarCNPJ( ACNPJ );
    if ErroMsg <> '' then
      raise EACBrSuframa.Create( 'Erro de validação: ' + sLineBreak + String( ErroMsg ) );
  end;

  Acao := TStringList.Create;
  Stream := TMemoryStream.Create;
  try
    if ACNPJ = '' then
    begin
      ParametrosConsulta :=
        '<con:consultarSituacaoInscsuf soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
          '<inscsuf>' + ACBrUtil.OnlyNumber( ASuframa ) + '</inscsuf>' +
        '</con:consultarSituacaoInscsuf>';
    end
    else
    begin
      ParametrosConsulta :=
        '<con:consultarSituacaoInscCnpj soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">' +
          '<cnpj>' + ACBrUtil.OnlyNumber( ACNPJ ) + '</cnpj>' +
          '<inscsuf>' + ACBrUtil.OnlyNumber( ASuframa ) + '</inscsuf>' +
        '</con:consultarSituacaoInscCnpj>';
    end;

    Acao.Text :=
      '<?xml version="1.0" encoding="utf-8"?>' +
      '<soapenv:Envelope ' +
        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
        'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
        'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
        'xmlns:con="http://consultas.ws.cadastro.fucapi.br">' +
        '<soapenv:Header/>' +
        '<soapenv:Body>' + ParametrosConsulta + '</soapenv:Body>' +
      '</soapenv:Envelope>';

    try
      Acao.SaveToStream(Stream);

      Self.HTTPSend.Clear;
      Self.HTTPSend.Document.LoadFromStream(Stream);
      Self.HTTPSend.Headers.Add( 'SOAPAction: "' + URL_WEBSERVICE + '"' );
      Self.HTTPPost(URL_WEBSERVICE);

      if ACNPJ <> '' then
        Retorno := String(SeparaDados(AnsiString(RespHTTP.Text), 'ns1:consultarSituacaoInscCnpjReturn'))
      else
        Retorno := String(SeparaDados(AnsiString(RespHTTP.Text), 'ns1:consultarSituacaoInscsufReturn'));

      if Retorno <> '' then
        FSituacao.Codigo := StrToInt(Retorno)
      else
      begin
        ErroCodigo := String( SeparaDados(AnsiString(RespHTTP.Text), 'faultcode') );
        if ErroCodigo <> EmptyStr then
        begin
          ErroMsg := String( SeparaDados(AnsiString(RespHTTP.Text), 'faultstring') );
          raise EACBrSuframa.Create(ErroCodigo + sLineBreak + '  - ' + ErroMsg);
        end;

        if SameText(String(RespHTTP), Acao.Text) then
        begin
          RespHTTP.Clear;
          raise EACBrSuframa.Create('Resposta do webservice não foi recebida.');
        end;
      end;

      if Assigned(fOnBuscaEfetuada) then
        OnBuscaEfetuada( Self );
    except
      on E: Exception do
      begin
        raise EACBrSuframa.Create(
          'Ocorreu o seguinte erro ao consumir o webService Suframa:' + sLineBreak +
          '  - ' + E.Message
        );
      end;
    end;
  finally
    Stream.Free;
    Acao.Free;
  end;
end;

end.

