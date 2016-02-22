{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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

{$I ACBr.inc}

unit pnfsNFSeW_SP;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pnfsNFSeW,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao;

type
  { TNFSeW_SP }

  TNFSeW_SP = class(TNFSeWClass)
  private
    FSituacao: String;
    FTipoRecolhimento: String;
  protected

    procedure GerarChaveRPS;
    procedure GerarIdentificacaoRPS;
    procedure GerarValoresServico;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;
    procedure GerarListaServicos;

    procedure GerarRPSSubstituido;
    procedure GerarPrestador;
    procedure GerarServicoValores;

    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarXML_SP;

   function AsciiToByte(const ABinaryString: AnsiString): String;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;

    property Situacao: String         read FSituacao;
    property TipoRecolhimento: String read FTipoRecolhimento;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do SP.                                                                }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_SP }

procedure TNFSeW_SP.GerarChaveRPS;
begin
  Gerador.wGrupoNFSe('ChaveRPS');
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoPrestador', 1, 11, 1, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcStr, '', 'SerieRPS'          , 1, 02, 1, NFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampoNFSe(tcStr, '', 'NumeroRPS'         , 1, 12, 1, NFSe.IdentificacaoRps.Numero, '');
  Gerador.wGrupoNFSe('/ChaveRPS');
end;

procedure TNFSeW_SP.GerarIdentificacaoRPS;
var
 TipoRPS: String;
begin
  TipoRPS := EnumeradoToStr(NFSe.IdentificacaoRps.Tipo,
                           ['RPS','RPS-M','RPS-C'],
                           [trRPS, trNFConjugada, trCupom]);

  Gerador.wCampoNFSe(tcStr, '', 'TipoRPS'      , 1, 05, 1, TipoRPS, '');
  Gerador.wCampoNFSe(tcDat, '', 'DataEmissao'  , 1, 10, 1, NFse.DataEmissao, '');
  Gerador.wCampoNFSe(tcStr, '', 'StatusRPS'    , 1, 01, 1, FSituacao, '');
  Gerador.wCampoNFSe(tcStr, '', 'TributacaoRPS', 1, 01, 1, TTributacaoRPSToStr(NFSe.TipoTributacaoRPS), '');
end;

procedure TNFSeW_SP.GerarValoresServico;
var
  ISSRetido: String;
begin
  Gerador.wCampoNFSe(tcDe2, '', 'ValorServicos', 1, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorDeducoes', 1, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorPIS'     , 1, 15, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS'  , 1, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorINSS'    , 1, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorIR'      , 1, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCSLL'    , 1, 15, 0, NFSe.Servico.Valores.ValorCsll, '');

  Gerador.wCampoNFSe(tcStr, '', 'CodigoServico'   , 1, 05, 1, OnlyNumber(NFSe.Servico.ItemListaServico), '');
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaServicos', 1, 05, 0, NFSe.Servico.Valores.Aliquota, '');

  ISSRetido := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                       ['false', 'true'], [stNormal, stRetencao]);

  Gerador.wCampoNFSe(tcStr, '', 'ISSRetido', 1, 05, 1, ISSRetido, '');
end;

procedure TNFSeW_SP.GerarTomador;
begin
  if OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) <> '' then
  begin
    Gerador.wGrupoNFSe('CPFCNPJTomador');
    Gerador.wCampoCNPJCPF('', '', OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj));
    Gerador.wGrupoNFSe('/CPFCNPJTomador');
  end;
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalTomador', 1, 08, 0, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal), '');
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoEstadualTomador' , 1, 19, 0, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), '');
  Gerador.wCampoNFSe(tcStr, '', 'RazaoSocialTomador'       , 1, 75, 0, NFSe.Tomador.RazaoSocial, '');

  Gerador.wGrupoNFSe('EnderecoTomador');
  Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouro',      0, 10, 0, NFSe.Tomador.Endereco.TipoLogradouro, '');
  Gerador.wCampoNFSe(tcStr, '', 'Logradouro',          1, 50, 0, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'NumeroEndereco',      1, 09, 0, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'ComplementoEndereco', 1, 30, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'Bairro',              1, 50, 0, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'Cidade',              1, 10, 0, NFSe.Tomador.Endereco.CodigoMunicipio, '');
  Gerador.wCampoNFSe(tcStr, '', 'UF',                  1, 02, 0, NFSe.Tomador.Endereco.UF, '');
  Gerador.wCampoNFSe(tcStr, '', 'CEP',                 1, 08, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wGrupoNFSe('/EnderecoTomador');

  Gerador.wCampoNFSe(tcStr, '', 'EmailTomador', 1, 75, 0, NFSe.Tomador.Contato.Email, '');
end;

procedure TNFSeW_SP.GerarIntermediarioServico;
var
  sISSRetidoInter: String;
begin
  if OnlyNumber(NFSe.IntermediarioServico.CpfCnpj) <> '' then
  begin
    Gerador.wGrupoNFSe('CPFCNPJIntermediario');
    Gerador.wCampoCNPJCPF('', '', OnlyNumber(NFSe.IntermediarioServico.CpfCnpj));
    Gerador.wGrupoNFSe('/CPFCNPJIntermediario');

    Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalIntermediario',  01, 08, 0, OnlyNumber(NFSe.IntermediarioServico.InscricaoMunicipal), '');

    sISSRetidoInter := EnumeradoToStr( NFSe.IntermediarioServico.IssRetido,
                                  ['false', 'true'], [stNormal, stRetencao]);

    Gerador.wCampoNFSe(tcStr, '', 'ISSRetidoIntermediario', 1, 05, 0, sISSRetidoInter, '');
    Gerador.wCampoNFSe(tcStr, '', 'EmailIntermediario'    , 1, 75, 0, NFSe.IntermediarioServico.EMail, '');
  end;
end;

procedure TNFSeW_SP.GerarListaServicos;
begin
  Gerador.wCampoNFSe(tcStr, '', 'Discriminacao', 1, 2000, 1, NFSe.Servico.Discriminacao, '');
end;


procedure TNFSeW_SP.GerarRPSSubstituido;
begin
  // Não definido
end;

procedure TNFSeW_SP.GerarPrestador;
begin
  // Não definido
end;

procedure TNFSeW_SP.GerarServicoValores;
begin
  // Não definido
end;

procedure TNFSeW_SP.GerarConstrucaoCivil;
begin
  // Não definido
end;

procedure TNFSeW_SP.GerarCondicaoPagamento;
begin
  // Não definido
end;

function TNFSeW_SP.AsciiToByte(const ABinaryString: AnsiString): String;
var
  I, L: Integer;
begin
  Result := '' ;
  L := Length(ABinaryString) ;
  for I := 1 to L do
    Result := Result + IntToStr(Ord(ABinaryString[I]));
end;

procedure TNFSeW_SP.GerarXML_SP;
var
  sAssinatura, sISSRetido, sCPFCNPJTomador, sIndTomador, sTomador,
  sCPFCNPJInter, sIndInter, sISSRetidoInter, sInter: String;
begin
  Gerador.Prefixo := '';
//  Gerador.wGrupoNFSe('RPS ' + FIdentificador + '="rps:' + NFSe.InfID.ID + '"');
  Gerador.wGrupoNFSe('RPS');

  FSituacao := EnumeradoToStr( NFSe.Status, ['N', 'C'], [srNormal, srCancelado]);

  FTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                       ['A', 'R'], [stNormal, stRetencao]);

  sISSRetido := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                ['N', 'S'], [stNormal, stRetencao]);

  // Tomador do Serviço
  sCPFCNPJTomador := OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);

  if Length(sCPFCNPJTomador) = 11 then
    sIndTomador := '1'
  else
    if Length(sCPFCNPJTomador) = 14 then
      sIndTomador := '2'
    else
      sIndTomador := '3';

  if sIndTomador <> '3' then
    sTomador := sIndTomador + Poem_Zeros(sCPFCNPJTomador, 14)
  else
    sTomador := '';

  // Prestador Intermediario
  sCPFCNPJInter := OnlyNumber(NFSe.IntermediarioServico.CpfCnpj);

  if Length(sCPFCNPJInter) = 11 then
    sIndInter := '1'
  else
    if Length(sCPFCNPJInter) = 14 then
      sIndInter := '2'
    else
      sIndInter := '3';

  sISSRetidoInter := EnumeradoToStr( NFSe.IntermediarioServico.IssRetido,
                                ['N', 'S'], [stNormal, stRetencao]);

  if sIndInter <> '3' then
    sInter := sIndInter + Poem_Zeros(sCPFCNPJInter, 14) + sISSRetidoInter
  else
    sInter := '';

  sAssinatura := Poem_Zeros(NFSe.Prestador.InscricaoMunicipal, 8) +
                 PadRight( NFSe.IdentificacaoRps.Serie, 5 , ' ') +
                 Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12) +
                 FormatDateTime('yyyymmdd',NFse.DataEmissao) +
                 TTributacaoRPSToStr(NFSe.TipoTributacaoRPS) +
                 Situacao +
                 sISSRetido +
                 Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorServicos)), 15 ) +
                 Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorDeducoes)), 15 ) +
                 Poem_Zeros(OnlyNumber(NFSe.Servico.ItemListaServico ), 5 ) +
                 sTomador +
                 sInter;

  sAssinatura := AsciiToByte(sAssinatura);
  sAssinatura := AsciiToHex(SHA1(sAssinatura));

  //  sAssinatura := AsciiToHex(MD5(SHA1(sAssinatura)));
//  sAssinatura := LowerCase(sAssinatura);

//  sAssinatura := AsciiToHex(sAssinatura);
//  sAssinatura := SHA1(sAssinatura);
//  sAssinatura := LowerCase(sAssinatura);

  Gerador.wCampoNFSe(tcStr, '', 'Assinatura', 1, 2000, 1, sAssinatura, '');

  GerarChaveRPS;
  GerarIdentificacaoRPS;
  GerarValoresServico;
  GerarTomador;
  GerarIntermediarioServico;
  GerarListaServicos;

  Gerador.wGrupoNFSe('/RPS');
end;

////////////////////////////////////////////////////////////////////////////////

constructor TNFSeW_SP.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_SP.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_SP.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.Opcoes.SuprimirDecimais := True;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  FNFSe.InfID.ID := FNFSe.IdentificacaoRps.Numero;

  GerarXML_SP;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NFSe.signature.DigestValue <> '') and
                (NFSe.signature.SignatureValue <> '') and
                (NFSe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NFSe.signature.DigestValue = '') and
                (NFSe.signature.SignatureValue = '') and
                (NFSe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FNFSe.signature.URI := FNFSe.InfID.ID;
      FNFSe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FNFSe.signature.GerarXMLNFSe;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                   FNFSe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Gerador.Opcoes.SuprimirDecimais := False;
  
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
