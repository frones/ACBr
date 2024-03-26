{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pcnCIOTW_Pamcard;

interface

uses
{$IFDEF FPC}
  LResources, 
  Controls, 
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes, 
  StrUtils,
  synacode, 
  ACBrConsts,
  pcnCIOTW, 
  pcnCIOTR,
  pcnConversao,
  pcnGerador, 
  pcnLeitor,
  pcnCIOT, 
  ACBrCIOTConversao;

type
  { TCIOTW_Pamcard }

  TCIOTW_Pamcard = class(TCIOTWClass)
  private
    FVersaoDF: TVersaoCIOT;
  protected
    procedure GerarViagem;
    procedure GerarImpostos;
    procedure GerarPagamentos;
    procedure GerarContratado;
    procedure GerarMotorista;
    procedure GerarDestinatario;
    procedure GerarContratante;
    procedure GerarSubContratante;
    procedure GerarConsignatario;
    procedure GerarTomadorServico;
    procedure GerarVeiculos;
  public
    constructor Create(ACIOTW: TCIOTW); override;

    property VersaoDF: TVersaoCIOT   read FVersaoDF write FVersaoDF;

    function GerarXml: Boolean; override;
  end;

const
  DSC_USUARIO = 'login: nome do usuário';
  DSC_SENHA = 'login: senha do usuário';
  DSC_CODATM = 'login: codigo AT&M';
  DSC_APLICACAO = 'Nome da Aplicação';
  DSC_ASSUNTO = 'Assunto do e-mail';
  DSC_REMETENTES = 'Remetentes do e-mail';
  DSC_DESTINATARIOS = 'Destinatários do e-mail';
  DSC_CORPO = 'Corpo do e-mail';
  DSC_CHAVE = 'Chave';
  DSC_CHAVERESP = 'Chave Resposta';

  NAME_SPACE_BASE = 'xmlns="http://schemas.ipc.adm.br/efrete/pef"';

  NAME_SPACE_EFRETE_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/objects"';
  NAME_SPACE_EFRETE_PEFOBTER_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/ObterOperacaoTransporteObjects"';
  NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarOperacaoTransporte"';
  NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarViagem"';
  NAME_SPACE_EFRETE_PEFADICIONAR_PAGAMENTOS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/AdicionarPagamento"';
  NAME_SPACE_EFRETE_PEFENCERRAR_OPERACAO = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/EncerrarOperacaoTransporte"';

  NAME_SPACE_EFRETE_PEFRETIFICAR_OBJECTS = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/RetificarOperacaoTransporte"';
  NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/pef/objects"';

  NAME_SPACE_EFRETE_VEICULOS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/veiculos/objects"';
  NAME_SPACE_EFRETE_MOTORISTAS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/motoristas/objects"';
  NAME_SPACE_EFRETE_PROPRIETARIOS_EFRETE = 'xmlns="http://schemas.ipc.adm.br/efrete/proprietarios/objects"';

implementation

constructor TCIOTW_Pamcard.Create(ACIOTW: TCIOTW);
begin
  inherited Create(ACIOTW);
end;

procedure TCIOTW_Pamcard.GerarSubContratante;
begin
  //É o transportador que contratar outro transportador para realização do transporte de
  //cargas para o qual fora anteriormente contratado, indicado no cadastramento da Operação de Transporte.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.AdicionarOperacao.Subcontratante.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupo('Subcontratante', 'AP135');
    Gerador.wCampo(tcStr, 'AP136', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.AdicionarOperacao.Subcontratante.NomeOuRazaoSocial);
    Gerador.wCampo(tcStr, 'AP137', 'CpfOuCnpj', 01, 01, 1, CIOT.AdicionarOperacao.Subcontratante.CpfOuCnpj);

    with CIOT.AdicionarOperacao.Subcontratante do
    begin
      Gerador.wGrupo('Endereco', 'AP138');
      Gerador.wCampo(tcStr, 'AP139', 'Bairro', 01, 01, 1, Endereco.Bairro);
      Gerador.wCampo(tcStr, 'AP140', 'Rua', 01, 01, 1, Endereco.Rua);
      Gerador.wCampo(tcStr, 'AP141', 'Numero', 01, 01, 1, Endereco.Numero);
      Gerador.wCampo(tcStr, 'AP142', 'Complemento', 01, 01, 1, Endereco.Complemento);
      Gerador.wCampo(tcStr, 'AP143', 'CEP', 01, 09, 1, Endereco.CEP);
      Gerador.wCampo(tcInt, 'AP144', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.wGrupo('/Endereco');
    end;

    Gerador.wCampo(tcStr, 'AP145', 'EMail', 01, 01, 1, CIOT.AdicionarOperacao.Subcontratante.EMail);

    with CIOT.AdicionarOperacao.Subcontratante.Telefones do
    begin
      Gerador.wGrupo('Telefones', 'AP146');

      Gerador.wGrupo('Celular '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP147');
      Gerador.wCampo(tcInt, 'AP148', 'DDD', 01, 02, 1, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP149', 'Numero', 08, 09, 1, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Celular');

      Gerador.wGrupo('Fixo '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP150');
      Gerador.wCampo(tcInt, 'AP151', 'DDD', 01, 02, 1, Fixo.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP152', 'Numero', 08, 09, 1, Fixo.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fixo');

      Gerador.wGrupo('Fax '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP153');
      Gerador.wCampo(tcInt, 'AP154', 'DDD', 01, 02, 1, Fax.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP155', 'Numero', 08, 09, 1, Fax.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fax');

      Gerador.wGrupo('/Telefones');
    end;

    Gerador.wCampo(tcStr, 'AP156', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.Subcontratante.ResponsavelPeloPagamento, true)));

    Gerador.wGrupo('/Subcontratante');
  end
  else
    Gerador.wCampo(tcStr, 'AP135', 'Subcontratante', 01, 01, 1, '');
end;

procedure TCIOTW_Pamcard.GerarTomadorServico;
begin
  //Pessoa (física ou jurídica) que contratou o frete pela transportadora.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
  if Length(Trim(CIOT.AdicionarOperacao.TomadorServico.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupo('TomadorServico', 'AP179');
    Gerador.wCampo(tcStr, 'AP180', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.AdicionarOperacao.TomadorServico.NomeOuRazaoSocial);
    Gerador.wCampo(tcStr, 'AP181', 'CpfOuCnpj', 01, 01, 1, CIOT.AdicionarOperacao.TomadorServico.CpfOuCnpj);

    with CIOT.AdicionarOperacao.TomadorServico do
    begin
      Gerador.wGrupo('Endereco', 'AP182');
      Gerador.wCampo(tcStr, 'AP183', 'Bairro', 01, 01, 1, Endereco.Bairro);
      Gerador.wCampo(tcStr, 'AP184', 'Rua', 01, 01, 1, Endereco.Rua);
      Gerador.wCampo(tcStr, 'AP185', 'Numero', 01, 01, 1, Endereco.Numero);
      Gerador.wCampo(tcStr, 'AP186', 'Complemento', 01, 01, 1, Endereco.Complemento);
      Gerador.wCampo(tcStr, 'AP187', 'CEP', 01, 09, 1, Endereco.CEP);
      Gerador.wCampo(tcInt, 'AP188', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.wGrupo('/Endereco');
    end;

    Gerador.wCampo(tcStr, 'AP189', 'EMail', 01, 01, 1, CIOT.AdicionarOperacao.TomadorServico.EMail);

    with CIOT.AdicionarOperacao.TomadorServico.Telefones do
    begin
      Gerador.wGrupo('Telefones', 'AP190');

      Gerador.wGrupo('Celular '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP191');
      Gerador.wCampo(tcInt, 'AP192', 'DDD', 01, 02, 1, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP193', 'Numero', 08, 09, 1, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Celular');

      Gerador.wGrupo('Fixo '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP194');
      Gerador.wCampo(tcInt, 'AP195', 'DDD', 01, 02, 1, Fixo.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP196', 'Numero', 08, 09, 1, Fixo.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fixo');

      Gerador.wGrupo('Fax '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP197');
      Gerador.wCampo(tcInt, 'AP198', 'DDD', 01, 02, 1, Fax.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP199', 'Numero', 08, 09, 1, Fax.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fax');

      Gerador.wGrupo('/Telefones');
    end;

    Gerador.wCampo(tcStr, 'AP200', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.TomadorServico.ResponsavelPeloPagamento, true)));

    Gerador.wGrupo('/TomadorServico');
  end
  else
    Gerador.wCampo(tcStr, 'AP179', 'TomadorServico ', 01, 01, 1, '');
end;

procedure TCIOTW_Pamcard.GerarConsignatario;
begin
  //Aquele que receberá as mercadorias transportadas em consignação,
  //indicado no cadastramento da Operação de Transporte ou nos respectivos documentos fiscais.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.AdicionarOperacao.Consignatario.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupo('Consignatario', 'AP157');
    Gerador.wCampo(tcStr, 'AP158', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.AdicionarOperacao.Consignatario.NomeOuRazaoSocial);
    Gerador.wCampo(tcStr, 'AP159', 'CpfOuCnpj', 01, 01, 1, CIOT.AdicionarOperacao.Consignatario.CpfOuCnpj);

    with CIOT.AdicionarOperacao.Consignatario do
    begin
      Gerador.wGrupo('Endereco', 'AP160');
      Gerador.wCampo(tcStr, 'AP161', 'Bairro', 01, 01, 1, Endereco.Bairro);
      Gerador.wCampo(tcStr, 'AP162', 'Rua', 01, 01, 1, Endereco.Rua);
      Gerador.wCampo(tcStr, 'AP163', 'Numero', 01, 01, 1, Endereco.Numero);
      Gerador.wCampo(tcStr, 'AP164', 'Complemento', 01, 01, 1, Endereco.Complemento);
      Gerador.wCampo(tcStr, 'AP165', 'CEP', 01, 09, 1, Endereco.CEP);
      Gerador.wCampo(tcInt, 'AP166', 'CodigoMunicipio', 01, 07, 1, Endereco.CodigoMunicipio);
      Gerador.wGrupo('/Endereco');
    end;

    Gerador.wCampo(tcStr, 'AP167', 'EMail', 01, 01, 1, CIOT.AdicionarOperacao.Consignatario.EMail);

    with CIOT.AdicionarOperacao.Consignatario.Telefones do
    begin
      Gerador.wGrupo('Telefones', 'AP168');

      Gerador.wGrupo('Celular '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP169');
      Gerador.wCampo(tcInt, 'AP170', 'DDD', 01, 02, 1, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP171', 'Numero', 08, 09, 1, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Celular');

      Gerador.wGrupo('Fixo '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP172');
      Gerador.wCampo(tcInt, 'AP173', 'DDD', 01, 02, 1, Fixo.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP174', 'Numero', 08, 09, 1, Fixo.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fixo');

      Gerador.wGrupo('Fax '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP175');
      Gerador.wCampo(tcInt, 'AP176', 'DDD', 01, 02, 1, Fax.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP177', 'Numero', 08, 09, 1, Fax.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fax');

      Gerador.wGrupo('/Telefones');
    end;

    Gerador.wCampo(tcStr, 'AP178', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.Consignatario.ResponsavelPeloPagamento, true)));

    Gerador.wGrupo('/Consignatario');
  end
  else
    Gerador.wCampo(tcStr, 'AP157', 'Consignatario', 01, 01, 1, '');
end;

procedure TCIOTW_Pamcard.GerarContratado;
begin
  //TAC ou seu Equiparado, que efetuar o transporte rodoviário de cargas por conta de terceiros e
  //mediante remuneração, indicado no cadastramento da Operação de Transporte.
  //Para o TipoViagem Frota o Contratado será a própria empresa que está declarando a operação.
  Gerador.wGrupo('Contratado '+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS, 'AP81');
  Gerador.wCampo(tcStr, 'AP82', 'CpfOuCnpj', 01, 01, 1, CIOT.AdicionarOperacao.Contratado.CpfOuCnpj);
  Gerador.wCampo(tcStr, 'AP83', 'RNTRC', 01, 01, 1, CIOT.AdicionarOperacao.Contratado.RNTRC);
  Gerador.wGrupo('/Contratado');
end;

procedure TCIOTW_Pamcard.GerarContratante;
begin
  Gerador.wGrupo('Contratante', 'AP112');
  Gerador.wCampo(tcStr, 'AP113', 'RNTRC', 01, 01, 1, CIOT.AdicionarOperacao.Contratante.RNTRC);
  Gerador.wCampo(tcStr, 'AP114', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.AdicionarOperacao.Contratante.NomeOuRazaoSocial);
  Gerador.wCampo(tcStr, 'AP115', 'CpfOuCnpj', 11, 14, 1, CIOT.AdicionarOperacao.Contratante.CpfOuCnpj);

  with CIOT.AdicionarOperacao.Contratante do
  begin
    Gerador.wGrupo('Endereco', 'AP116');
    Gerador.wCampo(tcStr, 'AP117', 'Bairro', 01, 01, 1, Endereco.Bairro);
    Gerador.wCampo(tcStr, 'AP118', 'Rua', 01, 01, 1, Endereco.Rua);
    Gerador.wCampo(tcStr, 'AP119', 'Numero', 01, 01, 1, Endereco.Numero);
    Gerador.wCampo(tcStr, 'AP120', 'Complemento', 01, 01, 1, Endereco.Complemento);
    Gerador.wCampo(tcStr, 'AP121', 'CEP', 01, 09, 1, Endereco.CEP);
    Gerador.wCampo(tcInt, 'AP122', 'CodigoMunicipio', 07, 07, 1, Endereco.CodigoMunicipio);
    Gerador.wGrupo('/Endereco');
  end;

  Gerador.wCampo(tcStr, 'AP123', 'EMail', 01, 01, 1, CIOT.AdicionarOperacao.Contratante.EMail);

  with CIOT.AdicionarOperacao.Contratante.Telefones do
  begin
    Gerador.wGrupo('Telefones', 'AP124');

    Gerador.wGrupo('Celular '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP125');
    Gerador.wCampo(tcInt, 'AP126', 'DDD', 01, 02, 0, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wCampo(tcInt, 'AP127', 'Numero', 08, 09, 0, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wGrupo('/Celular');

    Gerador.wGrupo('Fixo ' +NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP128');
    Gerador.wCampo(tcInt, 'AP129', 'DDD', 01, 02, 0, Fixo.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wCampo(tcInt, 'AP130', 'Numero', 08, 09, 0, Fixo.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wGrupo('/Fixo');

    Gerador.wGrupo('Fax '+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP131');
    Gerador.wCampo(tcInt, 'AP132', 'DDD', 01, 02, 0, Fax.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wCampo(tcInt, 'AP133', 'Numero', 08, 09, 0, Fax.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wGrupo('/Fax');

    Gerador.wGrupo('/Telefones');
  end;

  Gerador.wCampo(tcStr, 'AP134', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.Contratante.ResponsavelPeloPagamento, true)));

  Gerador.wGrupo('/Contratante');
end;

procedure TCIOTW_Pamcard.GerarDestinatario;
begin
  //Destinatário da carga.
  //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
  //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
  //Não esperado para TipoViagem Frota.
  if Length(Trim(CIOT.AdicionarOperacao.Destinatario.CpfOuCnpj)) > 0 then
  begin
    Gerador.wGrupo('Destinatario', 'AP90');
    Gerador.wCampo(tcStr, 'AP91', 'NomeOuRazaoSocial', 01, 01, 1, CIOT.AdicionarOperacao.Destinatario.NomeOuRazaoSocial);
    Gerador.wCampo(tcStr, 'AP92', 'CpfOuCnpj', 11, 14, 1, CIOT.AdicionarOperacao.Destinatario.CpfOuCnpj);

    with CIOT.AdicionarOperacao.Destinatario do
    begin
      Gerador.wGrupo('Endereco', 'AP93');
      Gerador.wCampo(tcStr, 'AP94', 'Bairro', 01, 01, 1, Endereco.Bairro);
      Gerador.wCampo(tcStr, 'AP95', 'Rua', 01, 01, 1, Endereco.Rua);
      Gerador.wCampo(tcStr, 'AP96', 'Numero', 01, 01, 1, Endereco.Numero);
      Gerador.wCampo(tcStr, 'AP97', 'Complemento', 01, 01, 1, Endereco.Complemento);
      Gerador.wCampo(tcStr, 'AP98', 'CEP', 08, 08, 1, Endereco.CEP);
      Gerador.wCampo(tcInt, 'AP99', 'CodigoMunicipio', 07, 07, 1, Endereco.CodigoMunicipio);
      Gerador.wGrupo('/Endereco');
    end;

    Gerador.wCampo(tcStr, 'AP100', 'EMail', 01, 01, 1, CIOT.AdicionarOperacao.Destinatario.EMail);

    with CIOT.AdicionarOperacao.Destinatario.Telefones do
    begin
      Gerador.wGrupo('Telefones', 'AP101');

      Gerador.wGrupo('Celular '+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP102');
      Gerador.wCampo(tcInt, 'AP103', 'DDD', 01, 02, 1, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP104', 'Numero', 08, 09, 1, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Celular');

      Gerador.wGrupo('Fixo '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP105');
      Gerador.wCampo(tcInt, 'AP106', 'DDD', 01, 02, 1, Fixo.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP107', 'Numero', 08, 09, 1, Fixo.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fixo');

      Gerador.wGrupo('Fax ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP108');
      Gerador.wCampo(tcInt, 'AP109', 'DDD', 01, 02, 1, Fax.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcInt, 'AP110', 'Numero', 08, 09, 1, Fax.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wGrupo('/Fax');

      Gerador.wGrupo('/Telefones');
    end;
    Gerador.wCampo(tcStr, 'AP111', 'ResponsavelPeloPagamento', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.Destinatario.ResponsavelPeloPagamento, true)), 'Informar se é o responsável pelo pagamento da Operação de Transporte. True = Sim. False = Não');

    Gerador.wGrupo('/Destinatario');
  end
  else
    Gerador.wCampo(tcStr, 'AP90', 'Destinatario', 01, 01, 1, '');
end;

procedure TCIOTW_Pamcard.GerarImpostos;
begin
  Gerador.wGrupo('Impostos', 'AP61');
  Gerador.wCampo(tcDe2, 'AP62', 'IRRF', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.IRRF, 'Valor destinado ao IRRF');
  Gerador.wCampo(tcDe2, 'AP63', 'SestSenat', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.SestSenat, 'Valor destinado ao SEST / SENAT');
  Gerador.wCampo(tcDe2, 'AP64', 'INSS', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.INSS, 'Valor destinado ao INSS.');
  Gerador.wCampo(tcDe2, 'AP65', 'ISSQN', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.ISSQN, 'Valor destinado ao ISSQN.');
  Gerador.wCampo(tcDe2, 'AP66', 'OutrosImpostos', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.OutrosImpostos, 'Valor destinado a outros impostos não previstos.');
  Gerador.wCampo(tcStr, 'AP67', 'DescricaoOutrosImpostos', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.DescricaoOutrosImpostos);
  Gerador.wGrupo('/Impostos');
end;

procedure TCIOTW_Pamcard.GerarMotorista;
begin
  //É o condutor do veículo que irá realizar a operação de transporte,
  //pode ser o proprietário do veículo ou não.
  Gerador.wGrupo('Motorista '+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS, 'AP84');
  Gerador.wCampo(tcStr, 'AP85', 'CpfOuCnpj', 01, 11, 1, CIOT.AdicionarOperacao.Motorista.CpfOuCnpj, 'CPF ou CNPJ do Motorista.');
  Gerador.wCampo(tcStr, 'AP86', 'CNH', 01, 11, 1, CIOT.AdicionarOperacao.Motorista.CNH);

  Gerador.wGrupo('Celular '+ NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP87');
  with CIOT.AdicionarOperacao.Motorista do
  begin
    Gerador.wCampo(tcInt, 'AP88', 'DDD', 01, 02, 1, Celular.DDD, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
    Gerador.wCampo(tcInt, 'AP89', 'Numero', 08, 09, 1, Celular.Numero, '', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
  end;

  Gerador.wGrupo('/Celular');
  Gerador.wGrupo('/Motorista');
end;

procedure TCIOTW_Pamcard.GerarPagamentos;
var
  i: Integer;
begin
  //Pagamentos registrados.
  //- Pode existir mais de 1 pagamento com uma mesma categoria (exceto para Quitacao).
  //- A soma dos pagamentos c/ categoria Adiantamento, deverá ter o mesmo valor apontado na
  //     tag TotalAdiantamento da tag Viagem/Valores, e neste caso, a tag Documento do pagamento
  //     deverá conter o mesmo valor da tag DocumentoViagem da tag Viagem .
  //- Se a viagem possuir a tag TotalQuitacao maior que zero, deverá ter um pagamento correspondente,
  //     com Categoria Quitacao e com o Documento o mesmo valor apontado na tag DocumentoViagem .
  for i := 0 to CIOT.AdicionarOperacao.Pagamentos.Count -1 do
  begin
    with CIOT.AdicionarOperacao.Pagamentos.Items[i] do
    begin
      Gerador.wGrupo('Pagamentos '+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS, 'AP68');
      Gerador.wCampo(tcStr, 'AP69', 'IdPagamentoCliente', 01, 01, 1, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente.');
      Gerador.wCampo(tcDat, 'AP70', 'DataDeLiberacao', 01, 01, 1, DataDeLiberacao, 'Data em que o pagamento será liberado para saque.');
      Gerador.wCampo(tcDe2, 'AP71', 'Valor', 01, 01, 1, Valor, 'Valor do pagamento.');
      Gerador.wCampo(tcStr, 'AP72', 'TipoPagamento', 01, 01, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcStr, 'AP73', 'Categoria', 01, 01, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, -Quitacao, -SemCategoria, -Frota ', True, NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
      Gerador.wCampo(tcStr, 'AP74', 'Documento', 01, 01, 1, Documento, 'Documento relacionado a viagem.');

      //Preenchimento obrigatório para o TipoPagamento TransferenciaBancaria. Não deve ser preenchido para TipoPagamento eFRETE.
      Gerador.wGrupo('InformacoesBancarias '+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP75');
      Gerador.wCampo(tcStr, 'AP76', 'InstituicaoBancaria', 01, 01, 1, InformacoesBancarias.InstituicaoBancaria, 'Código de compensação da instituição bancária que será realizado o pagamento. ');
      Gerador.wCampo(tcStr, 'AP77', 'Agencia', 01, 01, 1, InformacoesBancarias.Agencia, 'Agência na qual o contratado possui conta com dígito (se houver).');
      Gerador.wCampo(tcStr, 'AP78', 'Conta', 01, 01, 1, InformacoesBancarias.Conta, 'Conta do contratado com dígito. ');
      Gerador.wGrupo('/InformacoesBancarias');

      Gerador.wCampo(tcStr, 'AP79', 'InformacaoAdicional', 01, 01, 1, InformacaoAdicional);
      if Categoria = tcpFrota then
        Gerador.wCampo(tcStr, 'AP80', 'CnpjFilialAbastecimento', 01, 01, 1, CnpjFilialAbastecimento);

      Gerador.wGrupo('/Pagamentos');
    end;
  end;
end;

procedure TCIOTW_Pamcard.GerarVeiculos;
var
  i: Integer;
begin
  //Registro dos veículos participantes da operação de transporte.
  for i := 0 to CIOT.AdicionarOperacao.Veiculos.Count -1 do
  begin
    Gerador.wGrupo('Veiculos '+ NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS, 'AP201');
    Gerador.wCampo(tcStr, 'AP202', 'Placa', 01, 07, 1, CIOT.AdicionarOperacao.Veiculos.Items[I].Placa);
    Gerador.wGrupo('/Veiculos');
  end;
end;

procedure TCIOTW_Pamcard.GerarViagem;
var
  i, j: Integer;
  Item: TNotaFiscalCollectionItem;
begin
  Gerador.wGrupo('Viagens '+NAME_SPACE_EFRETE_PEFADICIONAR_OBJECTS, 'AP16');

  for I := 0 to CIOT.AdicionarOperacao.Viagens.Count -1 do
  begin
    with CIOT.AdicionarOperacao.Viagens.Items[I] do
    begin
      Gerador.wCampo(tcStr, 'AP17', 'DocumentoViagem', 01, 01, 1, DocumentoViagem, 'Exemplo: CT-e / Serie, CTRC / Serie, Ordem de Serviço.');
      Gerador.wCampo(tcInt, 'AP18', 'CodigoMunicipioOrigem', 01, 07, 1, CodigoMunicipioOrigem);
      Gerador.wCampo(tcInt, 'AP19', 'CodigoMunicipioDestino', 01, 07, 1, CodigoMunicipioDestino);

      Gerador.wGrupo('Valores '+NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP20');
      Gerador.wCampo(tcDe2, 'AP21', 'TotalOperacao', 01, 01, 1, Valores.TotalOperacao);
      Gerador.wCampo(tcDe2, 'AP22', 'TotalViagem', 01, 01, 1, Valores.TotalViagem);
      Gerador.wCampo(tcDe2, 'AP23', 'TotalDeAdiantamento', 01, 01, 1, Valores.TotalDeAdiantamento);
      Gerador.wCampo(tcDe2, 'AP24', 'TotalDeQuitacao', 01, 01, 1, Valores.TotalDeQuitacao);
      Gerador.wCampo(tcDe2, 'AP25', 'Combustivel', 01, 01, 1, Valores.Combustivel);
      Gerador.wCampo(tcDe2, 'AP26', 'Pedagio', 01, 01, 1, Valores.Pedagio);
      Gerador.wCampo(tcDe2, 'AP27', 'OutrosCreditos', 01, 01, 1, Valores.OutrosCreditos);
      Gerador.wCampo(tcStr, 'AP28', 'JustificativaOutrosCreditos', 01, 01, 1, Valores.JustificativaOutrosCreditos);
      Gerador.wCampo(tcDe2, 'AP29', 'Seguro', 01, 01, 1, Valores.Seguro);
      Gerador.wCampo(tcDe2, 'AP30', 'OutrosDebitos', 01, 01, 1, Valores.OutrosDebitos);
      Gerador.wCampo(tcStr, 'AP31', 'JustificativaOutrosDebitos', 01, 01, 1, Valores.JustificativaOutrosDebitos);
      Gerador.wGrupo('/Valores');

      Gerador.wCampo(tcStr, 'AP32', 'TipoPagamento' +NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE');

      Gerador.wGrupo('NotasFiscais', 'AP33');

      for J := 0 to NotasFiscais.Count -1 do
      begin
        Item := NotasFiscais[J];
        Gerador.wGrupo('NotaFiscal', 'AP34');
        Gerador.wCampo(tcStr, 'AP35', 'Numero', 01, 01, 1, Item.Numero);
        Gerador.wCampo(tcStr, 'AP36', 'Serie', 01, 01, 1, Item.Serie);
        Gerador.wCampo(tcDat, 'AP37', 'Data', 01, 01, 1, Item.Data);
        Gerador.wCampo(tcDe2, 'AP38', 'ValorTotal', 01, 01, 1, Item.ValorTotal);
        Gerador.wCampo(tcDe4, 'AP39', 'ValorDaMercadoriaPorUnidade', 01, 01, 1, Item.ValorDaMercadoriaPorUnidade);
        Gerador.wCampo(tcInt, 'AP40', 'CodigoNCMNaturezaCarga', 01, 04, 1, Item.CodigoNCMNaturezaCarga);
        Gerador.wCampo(tcStr, 'AP41', 'DescricaoDaMercadoria', 01, 01, 1, Item.DescricaoDaMercadoria, 'Descrição adicional ao código NCM.');
        Gerador.wCampo(tcStr, 'AP42', 'UnidadeDeMedidaDaMercadoria', 01, 01, 1, TpUnMedMercToStr(Item.UnidadeDeMedidaDaMercadoria));
        Gerador.wCampo(tcStr, 'AP43', 'TipoDeCalculo', 01, 01, 1, TpVgTipoCalculoToStr(Item.TipoDeCalculo));
        Gerador.wCampo(tcDe4, 'AP44', 'ValorDoFretePorUnidadeDeMercadoria', 01, 01, 1, Item.ValorDoFretePorUnidadeDeMercadoria);
        Gerador.wCampo(tcDe4, 'AP45', 'QuantidadeDaMercadoriaNoEmbarque', 01, 01, 1, Item.QuantidadeDaMercadoriaNoEmbarque);

        Gerador.wGrupo('ToleranciaDePerdaDeMercadoria', 'AP46');
        Gerador.wCampo(tcStr, 'AP47', 'Tipo', 01, 01, 1, TpProporcaoToStr(Item.ToleranciaDePerdaDeMercadoria.Tipo));
        Gerador.wCampo(tcDe2, 'AP48', 'Valor', 01, 01, 1, Item.ToleranciaDePerdaDeMercadoria.Valor);
        Gerador.wGrupo('/ToleranciaDePerdaDeMercadoria');

        if Item.DiferencaDeFrete.Tipo <> SemDiferenca then
        begin
          Gerador.wGrupo('DiferencaDeFrete', 'AP49');
          Gerador.wCampo(tcStr, 'AP50', 'Tipo', 01, 01, 1, TpDifFreteToStr(Item.DiferencaDeFrete.Tipo));
          Gerador.wCampo(tcStr, 'AP51', 'Base', 01, 01, 1, TpDiferencaFreteBCToStr(Item.DiferencaDeFrete.Base));

          Gerador.wGrupo('Tolerancia', 'AP52');
          Gerador.wCampo(tcStr, 'AP53', 'Tipo', 01, 01, 1, TpProporcaoToStr(Item.DiferencaDeFrete.Tolerancia.Tipo));
          Gerador.wCampo(tcDe2, 'AP54', 'Valor', 01, 01, 1, Item.DiferencaDeFrete.Tolerancia.Valor);
          Gerador.wGrupo('/Tolerancia');

          Gerador.wGrupo('MargemGanho', 'AP55');
          Gerador.wCampo(tcStr, 'AP56', 'Tipo', 01, 01, 1, TpProporcaoToStr(Item.DiferencaDeFrete.MargemGanho.Tipo));
          Gerador.wCampo(tcDe2, 'AP57', 'Valor', 01, 01, 1, Item.DiferencaDeFrete.MargemGanho.Valor);
          Gerador.wGrupo('/MargemGanho');

          Gerador.wGrupo('MargemPerda', 'AP58');
          Gerador.wCampo(tcStr, 'AP59', 'Tipo', 01, 01, 1, TpProporcaoToStr(Item.DiferencaDeFrete.MargemPerda.Tipo));
          Gerador.wCampo(tcDe2, 'AP60', 'Valor', 01, 01, 1, Item.DiferencaDeFrete.MargemPerda.Valor);
          Gerador.wGrupo('/MargemPerda');

          Gerador.wGrupo('/DiferencaDeFrete');
        end;
        Gerador.wGrupo('/NotaFiscal');
      end;
      Gerador.wGrupo('/NotasFiscais');
    end;
  end;

  Gerador.wGrupo('/Viagens');
end;

function TCIOTW_Pamcard.GerarXml: Boolean;
var
//  Prefixo, NameSpaceServico, NameSpaceBase: string;
//  Ok: Boolean;
  versao: Integer;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  // Carrega Layout que sera utilizado para gera o txt
  Gerador.LayoutArquivoTXT.Clear;
  Gerador.ArquivoFormatoTXT := '';
  versao := 1;
//  VersaoDF := DblToVersaoCIOT(Ok, CIOT.OperacaoTransporte.Versao);
//  versao := VersaoCIOTToInt(VersaoDF);

  case CIOT.Integradora.Operacao of
    opObterPdf:
      begin
//        Gerador.wGrupo('ObterOperacaoTransportePdfRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//        Gerador.wCampo(tcStr, '', 'CodigoIdentificacaoOperacao', 01, 30, 1, CIOT.OperacaoTransporte.NumeroCIOT, '');
////        Gerador.wTexto('<DocumentoViagem ' +   NAME_SPACE_EFRETE_OBJECTS + '>'++'</DocumentoViagem >');
//        Gerador.wTexto('<Integrador ' +   NAME_SPACE_EFRETE_OBJECTS + '>' + CIOT.Integradora.HashIntegrador + '</Integrador>');
////        Gerador.wTexto('<Token ' +   NAME_SPACE_EFRETE_OBJECTS + '>'++'</Token>');
//        Gerador.wTexto('<Versao ' +   NAME_SPACE_EFRETE_OBJECTS + '>1</Versao>');
//        Gerador.wGrupo('/ObterOperacaoTransportePdfRequest');
      end;
    opAdicionar:
      begin
        Gerador.wGrupo('AdicionarOperacaoTransporte ' + NAME_SPACE_BASE, '');

        Gerador.wGrupo('AdicionarOperacaoTransporteRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP01');

        Gerador.wCampo(tcStr, 'AP02', 'TipoViagem', 01, 01, 1, TipoViagemCIOTToStr(CIOT.AdicionarOperacao.TipoViagem));
        Gerador.wCampo(tcStr, 'AP03', 'Integrador', 01, 01, 1, CIOT.Integradora.Integrador);
        Gerador.wCampo(tcInt, 'AP04', 'Versao', 01, 01, 1, versao);
//        Gerador.wCampo(tcStr, 'AP05', 'Token', 01, 01, 1, '');  //ver de que forma pegar o tocken em caso de não usar certificado
//        Gerador.wCampo(tcBoolStr, 'AP06', 'EmissaoGratuita', 01, 01, 1, CIOT.AdicionarOperacao.EmissaoGratuita);
        if CIOT.AdicionarOperacao.TipoViagem <> Frota then
          Gerador.wCampo(tcStr, 'AP07', ' BloquearNaoEquiparado', 01, 01, 1, LowerCase(BoolToStr(CIOT.AdicionarOperacao.BloquearNaoEquiparado)));
        Gerador.wCampo(tcStr, 'AP08', 'MatrizCNPJ', 01, 14, 1, CIOT.AdicionarOperacao.MatrizCNPJ);
        Gerador.wCampo(tcStr, 'AP09', 'FilialCNPJ', 01, 01, 1, CIOT.AdicionarOperacao.FilialCNPJ);
        Gerador.wCampo(tcStr, 'AP10', 'IdOperacaoCliente', 01, 01, 1, CIOT.AdicionarOperacao.IdOperacaoCliente, 'Id / Chave primária da operação de transporte no sistema do Cliente.');
        if CIOT.AdicionarOperacao.TipoViagem <> TAC_Agregado then //Se TipoViagem for TAC_Agregado o campo não deve ser preenchido.
          Gerador.wCampo(tcDat, 'AP11', 'DataInicioViagem', 01, 01, 1, CIOT.AdicionarOperacao.DataInicioViagem)
        else
          Gerador.wCampo(tcStr, 'AP11', 'DataInicioViagem', 01, 01, 1, '');
        Gerador.wCampo(tcDat, 'AP12', 'DataFimViagem', 01, 01, 1, CIOT.AdicionarOperacao.DataFimViagem, 'Data prevista para o fim de viagem.');
        case CIOT.AdicionarOperacao.TipoViagem of
          Padrao: Gerador.wCampo(tcInt, 'AP13', 'CodigoNCMNaturezaCarga', 01, 04, 1, CIOT.AdicionarOperacao.CodigoNCMNaturezaCarga);
          TAC_Agregado: Gerador.wCampo(tcStr, 'AP13', 'CodigoNCMNaturezaCarga', 01, 01, 1, '');
        end;
        if CIOT.AdicionarOperacao.TipoViagem <> Frota then
          Gerador.wCampo(tcDe4, 'AP14', 'PesoCarga ', 01, 01, 1, CIOT.AdicionarOperacao.PesoCarga);

        if CIOT.AdicionarOperacao.TipoViagem = Padrao then
        begin
          Gerador.wCampo(tcStr, 'AP15', 'TipoEmbalagem', 01, 01, 1, TipoEmbalagemToStr(CIOT.AdicionarOperacao.TipoEmbalagem));
          //Adiciona a Viagem
          GerarViagem;
        end;

        //Adiciona Impostos
        if CIOT.AdicionarOperacao.TipoViagem <> Frota then
          GerarImpostos;

        //Adiciona os Pagamentos
        GerarPagamentos;

        //Adiciona Contratado
        GerarContratado;

        //Adiciona Motorista
        GerarMotorista;

        //Adiciona Destinatario
        if CIOT.AdicionarOperacao.TipoViagem <> Frota then
          GerarDestinatario;

        //Adiciona Contratante
        GerarContratante;

        if CIOT.AdicionarOperacao.TipoViagem <> Frota then
        begin
          //Adiciona SubContratante
          GerarSubContratante;

          //Adiciona Consignatario
          GerarConsignatario;

          //Adiciona TomadorServico
          GerarTomadorServico;
        end;

        //Adiciona Veículos
        GerarVeiculos;

        //Informar um CIOT (se existente) que esteja relacionado à operação de transporte.
        //Por exemplo: No caso da presença de um Subcontratante na operação de transporte informar
        //o CIOT onde o Subcontratante foi o Contratado.
        Gerador.wCampo(tcStr, 'AP203', 'CodigoIdentificacaoOperacaoPrincipal', 01, 01, 1, CIOT.AdicionarOperacao.CodigoIdentificacaoOperacaoPrincipal);

        Gerador.wGrupo('ObservacoesAoTransportador', 'AP204');
     //   Gerador.wCampo(tcStr, 'AP205', 'String', 01, 01, 1, CIOT.AdicionarOperacao.ObservacoesAoTransportador);
        Gerador.wGrupo('/ObservacoesAoTransportador');

        Gerador.wGrupo('ObservacoesAoCredenciado', 'AP206');
//        Gerador.wCampo(tcStr, 'AP207', 'String', 01, 01, 1, CIOT.AdicionarOperacao.ObservacoesAoCredenciado);
        Gerador.wGrupo('/ObservacoesAoCredenciado');

        Gerador.wCampo(tcStr, 'AP208', 'EntregaDocumentacao', 01, 01, 1, CIOT.AdicionarOperacao.EntregaDocumentacao);
        Gerador.wCampo(tcInt, 'AP209', 'QuantidadeSaques', 01, 01, 1, CIOT.AdicionarOperacao.QuantidadeSaques);
        Gerador.wCampo(tcInt, 'AP210', 'QuantidadeTransferencias', 01, 01, 1, CIOT.AdicionarOperacao.QuantidadeTransferencias);

        Gerador.wGrupo('/AdicionarAdicionarOperacaoRequest');

        Gerador.wGrupo('/AdicionarAdicionarOperacao');
      end;
    opRetificar:
      begin
//        Gerador.wGrupo('RetificarAdicionarOperacaoRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'WP01');
//
//        Gerador.wCampo(tcStr, 'WP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, FOperacaoTransporte.NumeroCIOT, '');
//        Gerador.wCampo(tcInt, 'WP03', 'CodigoMunicipioDestino', 01, 07, 1, FOperacaoTransporte.Viagens.Items[0].CodigoMunicipioDestino); //0001
//        Gerador.wCampo(tcInt, 'WP04', 'CodigoMunicipioOrigem', 01, 07, 1, FOperacaoTransporte.Viagens.Items[0].CodigoMunicipioOrigem); //0001
//        Gerador.wCampo(tcInt, 'WP05', 'CodigoNCMNaturezaCarga', 01, 04, 1, FOperacaoTransporte.CodigoNCMNaturezaCarga); //0001
//        Gerador.wCampo(tcDat, 'WP06', 'DataFimViagem', 01, 01, 1, FOperacaoTransporte.DataFimViagem); //0001
//        Gerador.wCampo(tcDat, 'WP07', 'DataInicioViagem', 01, 01, 1, FOperacaoTransporte.DataInicioViagem); //0001
//        Gerador.wCampo(tcStr, 'WP08', 'Integrador', 01, 01, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//        Gerador.wCampo(tcDe4, 'WP09', 'PesoCarga', 01, 01, 1, FOperacaoTransporte.PesoCarga); //0001
//        Gerador.wCampo(tcStr, 'WP10', 'Token', 01, 01, 1, '');
//
//        Gerador.wCampo(tcStr, 'WP13', 'Versao', 001, 001, 1, 1, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//
//        Gerador.wGrupo('Veiculos ' + NAME_SPACE_EFRETE_PEFRETIFICAR_OBJECTS, 'WP11');
//        for I := 0 to FOperacaoTransporte.Veiculos.Count -1 do
//        begin
//          with FOperacaoTransporte.Veiculos.Items[I] do
//            Gerador.wCampo(tcStr, 'WP12', 'Placa', 001, 001, 1, Placa, 'Placa do veículo conforme exemplo: AAA1234.');
//        end;
//        Gerador.wGrupo('/Veiculos');
//
//        Gerador.wGrupo('/RetificarOperacaoTransporteRequest');
      end;
    opCancelar:
      begin
        Gerador.wGrupo('CancelarOperacaoTransporteRequest', 'KP01');
//        Gerador.wCampo(tcStr, 'KP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, CIOT.AdicionarOperacao.NumeroCIOT);
        Gerador.wCampo(tcStr, 'KP03', 'Integrador', 01, 01, 1, CIOT.Integradora.Integrador);
//        Gerador.wCampo(tcStr, 'KP04', 'Motivo', 01, 01, 1, CIOT.AdicionarOperacao.Cancelamento.Motivo, '');
//        Gerador.wCampo(tcStr, 'KP05', 'Token ', 01, 01, 1, '');
        Gerador.wCampo(tcInt, 'KP06', 'Versao', 01, 01, 1, 1);
        Gerador.wGrupo('/CancelarOperacaoTransporteRequest');
      end;
    opAdicionarViagem:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupo('AdicionarViagemRequest' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampo(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'Versao', 001, 001, 1, 2, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wGrupo('Viagens ' + NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM, '');
//
//          for I := 0 to FOperacaoTransporte.Viagens.Count -1 do
//          begin
//            Gerador.wGrupo('Viagem');
//            with FOperacaoTransporte.Viagens.Items[I] do
//            begin
//              Gerador.wCampo(tcInt, 'AP133', 'CodigoMunicipioDestino', 001, 007, 1, CodigoMunicipioDestino);
//              Gerador.wCampo(tcInt, 'AP134', 'CodigoMunicipioOrigem', 001, 007, 1, CodigoMunicipioOrigem);
//              Gerador.wCampo(tcStr, 'AP135', 'DocumentoViagem', 001, 001, 1, DocumentoViagem, 'Exemplo: CT-e / Serie, CTRC / Serie, Ordem de Serviço.');
//
//              for J := 0 to NotasFiscais.Count -1 do
//              begin
//                with NotasFiscais.Items[J] do
//                begin
//                  Gerador.wGrupo('NotasFiscais');
//                  Gerador.wCampo(tcInt, 'AP137', 'CodigoNCMNaturezaCarga', 001, 004, 1, CodigoNCMNaturezaCarga);
//                  Gerador.wCampo(tcDat, 'AP138', 'Data', 001, 004, 1, Data);
//                  Gerador.wCampo(tcStr, 'AP139', 'DescricaoDaMercadoria', 001, 060, 1, DescricaoDaMercadoria, 'Descrição adicional ao código NCM.');
//                  Gerador.wCampo(tcStr, 'AP140', 'Numero', 001, 010, 1, Numero);
//                  Gerador.wCampo(tcDe3, 'AP141', 'QuantidadeDaMercadoriaNoEmbarque', 001, 010, 1, QuantidadeDaMercadoriaNoEmbarque);
//                  Gerador.wCampo(tcStr, 'AP142', 'Serie', 001, 001, 1, Serie);
//                  Gerador.wCampo(tcStr, 'AP143', 'TipoDeCalculo', 001, 001, 1, TpVgTipoCalculoToStr(TipoDeCalculo));
//                  Gerador.wGrupo('ToleranciaDePerdaDeMercadoria', 'AP144');
//                  Gerador.wCampo(tcStr, 'AP145', 'Tipo', 001, 001, 1, TpProporcaoToStr(ToleranciaDePerdaDeMercadoria.Tipo));
//                  Gerador.wCampo(tcDe2, 'AP146', 'Valor', 001, 001, 1, ToleranciaDePerdaDeMercadoria.Valor);
//                  Gerador.wGrupo('/ToleranciaDePerdaDeMercadoria');
//
//                  if DiferencaDeFrete.Tipo <> SemDiferenca then
//                  begin
//                    Gerador.wGrupo('DiferencaDeFrete', 'AP147');
//                    Gerador.wCampo(tcStr, 'AP148', 'Tipo', 001, 001, 1, TpDifFreteToStr(DiferencaDeFrete.Tipo));
//                    Gerador.wCampo(tcStr, 'AP149', 'Base', 001, 001, 1, TpDiferencaFreteBCToStr(DiferencaDeFrete.Base));
//                    Gerador.wGrupo('Tolerancia', 'AP150');
//                    Gerador.wCampo(tcStr, 'AP151', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.Tolerancia.Tipo));
//                    Gerador.wCampo(tcDe2, 'AP152', 'Valor', 001, 001, 1, DiferencaDeFrete.Tolerancia.Valor);
//                    Gerador.wGrupo('/Tolerancia');
//                    Gerador.wGrupo('MargemGanho', 'AP153');
//                    Gerador.wCampo(tcStr, 'AP154', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.MargemGanho.Tipo));
//                    Gerador.wCampo(tcDe2, 'AP155', 'Valor', 001, 001, 1, DiferencaDeFrete.MargemGanho.Valor);
//                    Gerador.wGrupo('/MargemGanho');
//                    Gerador.wGrupo('MargemPerda', 'AP156');
//                    Gerador.wCampo(tcStr, 'AP157', 'Tipo', 001, 001, 1, TpProporcaoToStr(DiferencaDeFrete.MargemPerda.Tipo));
//                    Gerador.wCampo(tcDe2, 'AP158', 'Valor', 001, 001, 1, DiferencaDeFrete.MargemPerda.Valor);
//                    Gerador.wGrupo('/MargemPerda');
//                    Gerador.wGrupo('/DiferencaDeFrete');
//                  end;
//
//                  Gerador.wCampo(tcStr, 'AP159', 'UnidadeDeMedidaDaMercadoria', 001, 001, 1, TpUnMedMercToStr(UnidadeDeMedidaDaMercadoria));
//                  Gerador.wCampo(tcDe2, 'AP159', 'ValorDaMercadoriaPorUnidade', 001, 001, 1, ValorDaMercadoriaPorUnidade);
//                  Gerador.wCampo(tcDe2, 'AP159', 'ValorDoFretePorUnidadeDeMercadoria', 001, 001, 1, ValorDoFretePorUnidadeDeMercadoria);
//                  Gerador.wCampo(tcDe2, 'AP159', 'ValorTotal', 001, 001, 1, ValorTotal);
//
//                  Gerador.wGrupo('/NotasFiscais');
//                end;
//              end;
//
//              Gerador.wGrupo('Valores ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP163');
//              with Valores do
//              begin
//                Gerador.wCampo(tcDe2, 'AP164', 'Combustivel', 001, 001, 1, Combustivel);
//                Gerador.wCampo(tcStr, 'AP165', 'JustificativaOutrosCreditos', 001, 001, 1, JustificativaOutrosCreditos);
//                Gerador.wCampo(tcStr, 'AP166', 'JustificativaOutrosDebitos', 001, 001, 1, JustificativaOutrosDebitos);
//                Gerador.wCampo(tcDe2, 'AP167', 'OutrosCreditos', 001, 001, 1, OutrosCreditos);
//                Gerador.wCampo(tcDe2, 'AP168', 'OutrosDebitos', 001, 001, 1, OutrosDebitos);
//                Gerador.wCampo(tcDe2, 'AP169', 'Pedagio', 001, 001, 1, Pedagio);
//                Gerador.wCampo(tcDe2, 'AP170', 'Seguro', 001, 001, 1, Seguro);
//                Gerador.wCampo(tcDe2, 'AP171', 'TotalDeAdiantamento', 001, 001, 1, TotalDeAdiantamento);
//                Gerador.wCampo(tcDe2, 'AP172', 'TotalDeQuitacao', 001, 001, 1, TotalDeQuitacao);
//                Gerador.wCampo(tcDe2, 'AP173', 'TotalOperacao', 001, 001, 1, TotalOperacao);
//                Gerador.wCampo(tcDe2, 'AP174', 'TotalViagem', 001, 001, 1, TotalViagem);
//              end;
//              Gerador.wGrupo('/Valores');
//            end;
//
//            Gerador.wGrupo('/Viagem');
//          end;
//
//          Gerador.wGrupo('/Viagens');
//
//          Gerador.wGrupo('Pagamentos ' + NAME_SPACE_EFRETE_PEFADICIONAR_VIAGEM, '');
//
//          for I := 0 to FOperacaoTransporte.Pagamentos.Count -1 do
//          begin
//            with FOperacaoTransporte.Pagamentos.Items[I] do
//            begin
//              Gerador.wGrupo('Pagamento');
//              Gerador.wCampo(tcStr, 'AP92', 'Categoria', 001, 001, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, Quitacao, -SemCategoria ', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampo(tcDat, 'AP93', 'DataDeLiberacao', 001, 001, 1, DataDeLiberacao);
//              Gerador.wCampo(tcStr, 'AP94', 'Documento', 001, 020, 1, Documento, 'Documento relacionado a viagem.');
//              Gerador.wCampo(tcStr, 'AP94', 'IdPagamentoCliente', 001, 020, 1, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//              Gerador.wCampo(tcStr, 'AP95', 'InformacaoAdicional', 001, 000, 0, InformacaoAdicional, '');
//
//              Gerador.wGrupo('InformacoesBancarias ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP97');
//              with InformacoesBancarias do
//              begin
//                Gerador.wCampo(tcStr, 'AP98', 'Agencia', 001, 001, 1, Agencia);
//                Gerador.wCampo(tcStr, 'AP99', 'Conta', 001, 001, 1, Conta);
//                Gerador.wCampo(tcStr, 'AP100', 'InstituicaoBancaria', 001, 001, 1, InstituicaoBancaria);
//              end;
//              Gerador.wGrupo('/InformacoesBancarias');
//
//              Gerador.wCampo(tcStr, 'AP101', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampo(tcDe2, 'AP102', 'Valor', 001, 020, 1, Valor, 'Valor do pagamento.');
//              Gerador.wGrupo('/Pagamento');
//            end;
//          end;
//
//          Gerador.wGrupo('/Pagamentos');
//
//          Gerador.wCampo(tcStr, '', 'NaoAdicionarParcialmente', 001, 001, 1, 'false', '');
//          Gerador.wGrupo('/AdicionarViagemRequest');
//        end;
      end;
    opAdicionarPagamento:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupo('AdicionarPagamentoRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampo(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'Versao', 001, 001, 1, 2, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wGrupo('Pagamentos ' + NAME_SPACE_EFRETE_PEFADICIONAR_PAGAMENTOS, '');
//
//          for I := 0 to FOperacaoTransporte.Pagamentos.Count -1 do
//          begin
//            with FOperacaoTransporte.Pagamentos.Items[I] do
//            begin
//              Gerador.wGrupo('Pagamento');
//              Gerador.wCampo(tcStr, 'AP92', 'Categoria', 001, 001, 1, TpCatPagToStr(Categoria), 'Categoria relacionada ao pagamento realizado. Restrita aos membros da ENUM: -Adiantamento, -Estadia, Quitacao, -SemCategoria ', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampo(tcDat, 'AP93', 'DataDeLiberacao', 001, 001, 1, DataDeLiberacao);
//              Gerador.wCampo(tcStr, 'AP94', 'Documento', 001, 020, 1, Documento, 'Documento relacionado a viagem.');
//              Gerador.wCampo(tcStr, 'AP94', 'IdPagamentoCliente', 001, 020, 1, IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//              Gerador.wCampo(tcStr, 'AP95', 'InformacaoAdicional', 001, 000, 0, InformacaoAdicional, '');
//
//              Gerador.wGrupo('InformacoesBancarias ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE, 'AP97');
//              with InformacoesBancarias do
//              begin
//                Gerador.wCampo(tcStr, 'AP98', 'Agencia', 001, 001, 1, Agencia);
//                Gerador.wCampo(tcStr, 'AP99', 'Conta', 001, 001, 1, Conta);
//                Gerador.wCampo(tcStr, 'AP100', 'InstituicaoBancaria', 001, 001, 1, InstituicaoBancaria);
//              end;
//              Gerador.wGrupo('/InformacoesBancarias');
//
//              Gerador.wCampo(tcStr, 'AP101', 'TipoPagamento', 001, 020, 1, TpPagamentoToStr(TipoPagamento), 'Tipo de pagamento que será usado pelo contratante. Restrito aos itens da enum: -TransferenciaBancaria -eFRETE', ' ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//              Gerador.wCampo(tcDe2, 'AP102', 'Valor', 001, 020, 1, Valor, 'Valor do pagamento.');
//              Gerador.wGrupo('/Pagamento');
//            end;
//          end;
//
//          Gerador.wGrupo('/Pagamentos');
//          Gerador.wGrupo('/AdicionarPagamentoRequest');
//        end;
      end;
    opCancelarPagamento:
      begin
//        if FOperacaoTransporte.TipoViagem = TAC_Agregado then
//        begin
//          Gerador.wGrupo('CancelarPagamentoRequest ' + NAME_SPACE_EFRETE_OPERACAOTRANSPORTE_EFRETE);
//          Gerador.wCampo(tcStr, '', 'Integrador', 001, 001, 1, TAmsCIOT( FOperacaoTransporte.Owner ).Configuracoes.Integradora.Identificacao, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'Versao', 001, 001, 1, 1, '', ' ' + NAME_SPACE_EFRETE_OBJECTS);
//          Gerador.wCampo(tcStr, '', 'CodigoIdentificacaoOperacao', 001, 030, 1, FOperacaoTransporte.NumeroCIOT, '');
//          Gerador.wCampo(tcStr, '', 'IdPagamentoCliente', 001, 020, 1, FOperacaoTransporte.Cancelamento.IdPagamentoCliente, 'Identificador do pagamento no sistema do Cliente. ');
//          Gerador.wCampo(tcStr, 'KP04', 'Motivo', 001, 001, 1, FOperacaoTransporte.Cancelamento.Motivo, '');
//          Gerador.wGrupo('/CancelarPagamentoRequest');
//        end;
      end;
    opEncerrar:
      begin
        Gerador.wGrupo('EncerrarOperacaoTransporteRequest', 'QP01');

//        Gerador.wCampo(tcStr, 'QP02', 'CodigoIdentificacaoOperacao', 01, 01, 1, CIOT.AdicionarOperacao.NumeroCIOT);
        Gerador.wCampo(tcDe6, 'QP03', 'PesoCarga', 01, 01, 1, CIOT.AdicionarOperacao.PesoCarga, 'Peso da carga que foi transportado.');

        Gerador.wGrupo('Impostos', 'QP04');
        Gerador.wCampo(tcStr, 'QP05', 'DescricaoOutrosImpostos', 01, 01, 1, CIOT.AdicionarOperacao.Impostos.DescricaoOutrosImpostos);
        Gerador.wCampo(tcDe2, 'QP06', 'INSS', 01, 20, 1, CIOT.AdicionarOperacao.Impostos.INSS, 'Valor destinado ao INSS. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampo(tcDe2, 'QP07', 'IRRF', 01, 20, 1, CIOT.AdicionarOperacao.Impostos.IRRF, 'Valor destinado ao IRRF. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampo(tcDe2, 'QP08', 'ISSQN', 01, 20, 1, CIOT.AdicionarOperacao.Impostos.ISSQN, 'Valor destinado ao ISSQN. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampo(tcDe2, 'QP09', 'OutrosImpostos', 01, 20, 1, CIOT.AdicionarOperacao.Impostos.OutrosImpostos, 'Valor destinado a outros impostos não previstos. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wCampo(tcDe2, 'QP10', 'SestSenat', 01, 20, 1, CIOT.AdicionarOperacao.Impostos.SestSenat, 'Valor destinado ao SEST / SENAT. Este valor deverá fazer parte do valor de Adiantamento ou do valor de Quitação.');
        Gerador.wGrupo('/Impostos');

        Gerador.wCampo(tcStr, 'QP11', 'Integrador', 01, 01, 1, CIOT.Integradora.Integrador);
//        Gerador.wCampo(tcStr, 'QP12', 'Token', 01, 01, 1, '');
        Gerador.wCampo(tcInt, 'QP13', 'Versao', 01, 01, 1, 1);
        Gerador.wCampo(tcStr, 'QP14', 'QuantidadeSaques', 01, 01, 1, CIOT.AdicionarOperacao.QuantidadeSaques);
        Gerador.wCampo(tcStr, 'QP15', 'QuantidadeTransferencia', 01, 01, 1, CIOT.AdicionarOperacao.QuantidadeTransferencias);

        Gerador.wGrupo('/EncerrarOperacaoTransporteRequest');
      end;
  end;

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
