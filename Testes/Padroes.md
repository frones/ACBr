# ACBrTests

## Introdução

Algumas informações úteis ao criar testes unitários. Não são regras.

## Como nomear unidades de classes de testes (Delphi/Lazarus)

Podem ser nomeadas com o nome da classe e o sufixo TestsUnit por exemplo:

* [NomeDaClasseTestsUnit]

## Como nomear as classes de testes

As classes de teste podem ser nomeadas com o nome da classe e o sufixo Testes (ou Tests) por exemplo:

* [NomeDaClasseTestes]
* [NomeDaClasseTests]

## Como nomear os testes em si

Parece haver um consenso que os nomes dos testes devem:

1. Ser curto, mas descritivo o suficiente para identificá-lo mesmo por quem não está acostumado com os testes;
2. Descrever, se possível, a **ação**, o **estado** do objeto testado e o **objetivo** do teste;
3. Descrever o resultado esperado;

Mas também queremos que os testes sobrevivam ao tempo. Isto é, se uma classe ou método for renomeada, não gostaríamos que nosso teste também precise ser renomeado para fazer sentido.

Assim, ao invés de pensar nas entidades (classes, functions, etc..) que estão sendo testadas, pense em como descreveria o teste para uma pessoa que não é programador mas entende o "domínio" do problema. Por exemplo, se está criando testes para sua classe de vendas, como explicaria o teste para um vendedor. Se é uma classe de cálculo de impostos, como explicaria o teste para um contador.

Dessa forma alguns exemplos de nomes de testes são:

* Purchase_without_funds_is_not_possible
* Delivery_with_a_past_date_is_invalid
* Should_Increase_Balance_When_Deposit_Is_Made
* Add_credit_updates_customer_balance

### **Quando não consigo pensar num bom nome**

Por outro lado, quando se fica na dúvida sobre como nomear os testes, pode ser útil seguir o padrão abaixo.
Esse padrão resiste menos a refactorings, mas talvez é bem útil para criar testes com nomes eficazes.

[UnidadeDeTrabalho_EstadoSendoTestado_ResultadoEsperado]

Neste caso:

* **UnidadeDeTrabalho** pode ser uma entidade, método, uma classe ou várias classes. Mas representa o que está sendo testado neste teste específico. Deve-se tomar cuidado ao incluir o nome da entidade/método no teste, em especial caso exista alguma possibilidade de este método ser renomeado depois.
* **EstadoSendoTestado** descreve as condições do teste ou a ação da unidade do trabalho
* **ResultadoEsperado** descreve então como aquela unidade de trabalho deve se comportar

Isso sugere nomes como:

* WEBServer_LoginComSenhaVazia_DeveFalhar
* WEBServer_LoginComUsuarioVazio_DeveFalhar
* WEBServer_LoginComSenhaeUsuarioVazios_DeveFalhar
* Soma_NumeroNegativoNo1oParametro_GeraException
* Soma_ValoresSimples_SaoCalculados

## Fontes

Alguns artigos consultados:

* <https://enterprisecraftsmanship.com/posts/you-naming-tests-wrong/>
* <https://stackoverflow.com/questions/155436/unit-test-naming-best-practices>
* <http://osherove.com/blog/2005/4/3/naming-standards-for-unit-tests.html>
* <http://testing.bredex.de/naming-conventions-for-test-cases.html>
* <https://code.google.com/p/robotframework/wiki/HowToWriteGoodTestCases#Test_case_names>
* <http://sysgears.com/articles/the-art-of-writing-effective-and-transparent-test-cases/>
