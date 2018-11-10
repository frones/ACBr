unit ACBrLibMDFeTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TTestACBrMDFeLib }

  TTestACBrMDFeLib = class(TTestCase)
  published
    procedure Test_MDFe_Inicializar_Com_DiretorioInvalido;
    procedure Test_MDFe_Inicializar;
    procedure Test_MDFe_Inicializar_Ja_Inicializado;
    procedure Test_MDFe_Finalizar;
    procedure Test_MDFe_Finalizar_Ja_Finalizado;
    procedure Test_MDFe_Nome_Obtendo_LenBuffer;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Identico;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Maior;
    procedure Test_MDFe_Nome_Lendo_Buffer_Tamanho_Menor;
    procedure Test_MDFe_Versao;
    procedure Test_MDFe_ConfigLerValor;
    procedure Test_MDFe_ConfigGravarValor;

    procedure Test_MDFe_StatusServico;

    procedure Test_MDFe_LimparLista;
    {
    procedure Test_MDFe_CarregarXML;
    procedure Test_MDFe_Imprimir;
    procedure Test_MDFe_ImprimirPDF;

    procedure Test_MDFe_CarregarINI;
    procedure Test_MDFe_Assinar;
    procedure Test_MDFe_Validar;
    procedure Test_MDFe_ValidarRegrasdeNegocios;
    procedure Test_MDFe_VerificarAssinatura;

    procedure Test_MDFe_Enviar;
    procedure Test_MDFe_Consultar;
    procedure Test_MDFe_Cancelar;
    }
    procedure Test_MDFe_EnviarEmail;
    {
    procedure Test_MDFe_EnviarEvento;
    procedure Test_MDFe_EnviarEmailEvento;
    procedure Test_MDFe_ImprimirEvento;
    procedure Test_MDFe_ImprimirEventoPDF;

    procedure Test_MDFe_DistribuicaoDFePorUltNSU;
    procedure Test_MDFe_DistribuicaoDFePorNSU;
    }
  end;

implementation

uses
  ACBrLibMDFeStaticImport, ACBrLibMDFeConsts, ACBrLibConsts, ACBrLibComum;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar_Com_DiretorioInvalido;
begin
  AssertEquals(ErrDiretorioNaoExiste, MDFe_Inicializar('C:\NAOEXISTE\ACBrLib.ini',''));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar;
begin
  AssertEquals(ErrOk, MDFe_Inicializar('',''));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Inicializar_Ja_Inicializado;
begin
  AssertEquals(ErrOk, MDFe_Inicializar('',''));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Finalizar;
begin
  AssertEquals(ErrOk, MDFe_Finalizar());
end;

procedure TTestACBrMDFeLib.Test_MDFe_Finalizar_Ja_Finalizado;
begin
  AssertEquals(ErrOk, MDFe_Finalizar());
end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Obtendo_LenBuffer;
var
  Bufflen: Integer;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, MDFe_Nome(Nil, Bufflen));
  AssertEquals(Length(CLibMDFeNome), Bufflen);
end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Identico;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibMDFeNome);
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMDFeNome), Bufflen);
  AssertEquals(CLibMDFeNome, AStr);
end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Maior;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := Length(CLibMDFeNome)*2;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_Nome(PChar(AStr), Bufflen));
  AStr := copy(AStr, 1, Bufflen);
  AssertEquals(Length(CLibMDFeNome), Bufflen);
  AssertEquals(CLibMDFeNome, AStr);
end;

procedure TTestACBrMDFeLib.Test_MDFe_Nome_Lendo_Buffer_Tamanho_Menor;
var
  AStr: String;
  Bufflen: Integer;
begin
  Bufflen := 4;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_Nome(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMDFeNome), Bufflen);
  AssertEquals(copy(CLibMDFeNome,1,4), AStr);
end;

procedure TTestACBrMDFeLib.Test_MDFe_Versao;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 0;
  AssertEquals(ErrOk, MDFe_Versao(Nil, Bufflen));
  AssertEquals(Length(CLibMDFeVersao), Bufflen);

  // Lendo a resposta //
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_Versao(PChar(AStr), Bufflen));
  AssertEquals(Length(CLibMDFeVersao), Bufflen);
  AssertEquals(CLibMDFeVersao, AStr);
end;

procedure TTestACBrMDFeLib.Test_MDFe_ConfigLerValor;
var
  Bufflen: Integer;
  AStr: String;
begin
  // Obtendo o Tamanho //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_ConfigLerValor(CSessaoVersao, CLibMDFeNome, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals(CLibMDFeVersao, AStr);
end;

procedure TTestACBrMDFeLib.Test_MDFe_ConfigGravarValor;
var
  Bufflen: Integer;
  AStr, Senha: String;
begin
  // Gravando o valor
  AssertEquals('Erro ao Mudar configuração', ErrOk, MDFe_ConfigGravarValor(CSessaoPrincipal, CChaveLogNivel, '4'));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_ConfigLerValor(CSessaoPrincipal, CChaveLogNivel, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Mudar configuração', '4', AStr);

  Senha := StringToB64Crypt('imag2013', '');

  AssertEquals('Erro ao Configurar Senha', ErrOk,
   MDFe_ConfigGravarValor(CSessaoEmail, CChaveSenha, PChar(Senha)));

  // Checando se o valor foi atualizado //
  Bufflen := 255;
  AStr := Space(Bufflen);
  AssertEquals(ErrOk, MDFe_ConfigLerValor(CSessaoEmail, CChaveSenha, PChar(AStr), Bufflen));
  AStr := copy(AStr,1,Bufflen);
  AssertEquals('Erro ao Configurar Senha', 'imag2013', B64CryptToString(AStr, ''));
end;

procedure TTestACBrMDFeLib.Test_MDFe_StatusServico;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a consulta ao Status de Serviço
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao consultar ao Status de Serviço', ErrOk, MDFe_StatusServico(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrMDFeLib.Test_MDFe_LimparLista;
begin
  // Iniciando a Limpeza da Lista de MDF-e
  AssertEquals('Erro ao limpar a lista de MDF-e', ErrOk, MDFe_LimparLista);
end;
{
procedure TTestACBrMDFeLib.Test_MDFe_CarregarXML;
begin
  // Iniciando o Carregamento do XML do MDF-e
  AssertEquals('Erro ao carregar o XML do MDF-e', ErrOk,
   MDFe_CarregarXML('C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\28140417957142000144580170000000031895518397-mdfe.xml'));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Imprimir;
begin
  // Iniciando a Impressão do DAMDFe
  AssertEquals('Erro ao Imprimir o DAMDFe', ErrOk, MDFe_Imprimir);
end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirPDF;
begin
  // Iniciando a geração do PDF do DAMDFe
  AssertEquals('Erro ao gerar o PDF do DAMDFe', ErrOk, MDFe_ImprimirPDF);
end;

procedure TTestACBrMDFeLib.Test_MDFe_CarregarINI;
begin
  Test_MDFe_LimparLista;

  // Iniciando o Carregamento do INI do MDF-e
 // AssertEquals('Erro ao carregar o INI do MDF-e', ErrOk,
 //  MDFe_CarregarINI('C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\Modelo-MDFe.ini'));
end;

procedure TTestACBrMDFeLib.Test_MDFe_Assinar;
begin
  // Iniciando a Assinatura
//  AssertEquals('Erro ao Assinar', ErrOk, MDFe_Assinar);
end;

procedure TTestACBrMDFeLib.Test_MDFe_Validar;
begin
  // Iniciando a Validação
  AssertEquals('Erro ao Validar', ErrOk, MDFe_Validar);
end;

procedure TTestACBrMDFeLib.Test_MDFe_ValidarRegrasdeNegocios;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Validação de Regras de Negócios
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Validar regras de negócio', ErrOk,
    MDFe_ValidarRegrasdeNegocios(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrMDFeLib.Test_MDFe_VerificarAssinatura;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Verificação de Assinatura
  Resposta := '';
  Tamanho := 0;

  AssertEquals('Erro ao Verificar assinatura', ErrOk,
    MDFe_VerificarAssinatura(Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
end;

procedure TTestACBrMDFeLib.Test_MDFe_Enviar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando o Envio do Lote de MDF-e
  Resposta := '';
  Tamanho := 0;
  (*
  AssertEquals('Erro ao Enviar Lote de MDF-e', ErrOk,
    MDFe_Enviar(1, True, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  *)
end;

procedure TTestACBrMDFeLib.Test_MDFe_Consultar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_MDFe_LimparLista;

  // Iniciando a Consulta
  Resposta := '';
  Tamanho := 0;
  (*
  AssertEquals('Erro ao Consultar o MDF-e', ErrOk,
    MDFe_Consultar('C:\ERP\XML\201808\MDFe\35180804550110000188570010000009491283342822-MDFe.xml', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  *)
end;

procedure TTestACBrMDFeLib.Test_MDFe_Cancelar;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  Test_MDFe_LimparLista;

  // Iniciando o Cancelamento
  Resposta := '';
  Tamanho := 0;
  (*
  AssertEquals('Erro ao Cancelar o MDF-e', ErrOk,
    MDFe_Cancelar('35180804550110000188570010000009491283342822',
                 'Desacordo comercial', '04550110000188', 1, Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  *)
end;
}
procedure TTestACBrMDFeLib.Test_MDFe_EnviarEmail;
var
  Path, ArqMDFe: String;
begin
  // Iniciando o envio do e-mail
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';

  AssertEquals('Erro ao enviar o e-mail', ErrOk,
    MDFe_EnviarEmail('italo.jurisato@gmail.com', PChar(ArqMDFe), True,
      'Teste de envio', '', '', 'Em anexo o MDF-e') );
end;
{
procedure TTestACBrMDFeLib.Test_MDFe_EnviarEvento;
begin
  //a
end;

procedure TTestACBrMDFeLib.Test_MDFe_EnviarEmailEvento;
var
  Path, ArqMDFe, ArqEvento: String;
begin
  // Iniciando o envio do evento por email
  (*
  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  ArqMDFe := Path + '28140417957142000144570170000000311556600342-MDFe.xml';
  ArqEvento := Path + '2814041795714200014457017000000031155660034211011001-procEventoMDFe.xml';

  AssertEquals('Erro ao enviar email do evento', ErrOk,
    MDFe_EnviarEmailEvento('italo.jurisato@gmail.com', PChar(ArqEvento),
      PChar(ArqMDFe), True, 'Evento', '', '',
      'Teste de envio de evento por email.'));
  *)
end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirEvento;
var
  Path, ArqMDFe, ArqEvento: String;
begin
  // Iniciando a Impressão do Evento

  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';
  ArqEvento := Path + '2814041795714200014458017000000003189551839711011201-procEventoMDFe.xml';

  AssertEquals('Erro ao imprimir o evento', ErrOk,
    MDFe_ImprimirEvento(PChar(ArqMDFe), PChar(ArqEvento)));
end;

procedure TTestACBrMDFeLib.Test_MDFe_ImprimirEventoPDF;
var
  Path, ArqMDFe, ArqEvento: String;
begin
  // Iniciando a geração do PDF do Evento

  Path := 'C:\ACBr\trunk2\Projetos\ACBrLib\Testes\MDFe\bin\';
  ArqMDFe := Path + '28140417957142000144580170000000031895518397-mdfe.xml';
  ArqEvento := Path + '2814041795714200014458017000000003189551839711011201-procEventoMDFe.xml';

  AssertEquals('Erro ao gerar o PDF do evento', ErrOk,
    MDFe_ImprimirEventoPDF(PChar(ArqMDFe), PChar(ArqEvento)));
end;

procedure TTestACBrMDFeLib.Test_MDFe_DistribuicaoDFePorUltNSU;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Consulta no WebServices DistribuicaoDFe
  Resposta := '';
  Tamanho := 0;
  (*
  AssertEquals('Erro ao Consultar o DistribuicaoDFePorUltNSU', ErrOk,
    MDFe_DistribuicaoDFePorUltNSU('04550110000188', '0', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  *)
end;

procedure TTestACBrMDFeLib.Test_MDFe_DistribuicaoDFePorNSU;
var
  Resposta: PChar;
  Tamanho: Longint;
begin
  // Iniciando a Consulta no WebServices DistribuicaoDFe
  Resposta := '';
  Tamanho := 0;
  (*
  AssertEquals('Erro ao Consultar o DistribuicaoDFePorNSU', ErrOk,
    MDFe_DistribuicaoDFePorNSU('04550110000188', '100', Resposta, Tamanho));

  AssertEquals('Resposta= ' + AnsiString(Resposta), '', '');
  AssertEquals('Tamanho= ' + IntToStr(Tamanho), '', '');
  *)
end;
}
initialization
  RegisterTest(TTestACBrMDFeLib);

end.

