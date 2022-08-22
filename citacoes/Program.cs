using System;
using System.Text;
using System.Threading;
using citacoes.Base;
using OpenQA.Selenium;
using OpenQA.Selenium.Support.UI;
using SeleniumExtras.WaitHelpers;

namespace citacoes
{
    class Program
    {
        static void Main(string[] args)
        {
            var url = "https://scholar.google.com.br/scholar?lr=lang_en&hl=pt-BR&as_sdt=2005&sciodt=0,5&cites=9460237839723560213&scipsc=&start={0}";

            var selenium = new BaseSelenium();

            var csv = new StringBuilder();
            csv.AppendLine("title;tem_contexto_dissertação");

            var totalPage = Convert.ToInt32(Math.Ceiling(73D / 10D));

            for (int i = 0; i < totalPage; i++)
            {
                //Pega os dados do Buse & Weimer
                selenium.Navegate(String.Format(url, (i*10)));
                var wait = new WebDriverWait(selenium.Driver, TimeSpan.FromSeconds(5));

                var divResult = wait.Until(ExpectedConditions.ElementIsVisible(By.Id("gs_res_ccl_mid"))).FindElements(By.ClassName("gs_scl"));

                if (divResult.Count > 0)
                {
                    Console.WriteLine($"entrou {i}");

                    foreach (var item in divResult)
                    {
                        try
                        {
                            var title = item.FindElement(By.XPath("div[@class='gs_ri']/h3/a"));
                            csv.AppendLine($"{title.Text};0");
                        }
                        catch { }

                    }
                }

                Thread.Sleep(new Random().Next(3000, 15000));
            }

            Console.WriteLine(csv.ToString());
            Console.ReadLine();
            selenium.Close();
        }
    }
}
