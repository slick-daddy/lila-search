package lila.search

import lila.search.team.Team
import weaver.*

object TeamQueryTest extends FunSuite:

  private def queryString(text: String) = Team(text).searchDef(From(0), Size(10)).query.get.toString

  // A team name is analyzed into several tokens (e.g. "SF-Schönwald" -> [sf, schonwald]).
  // Without operator=AND the multi_match ORs them, so any team containing just "sf" matches.
  test("a single hyphenated term requires all of its tokens"):
    expect(queryString("SF-Schönwald").contains("Some(And)"))

  // AND is a no-op for a single token ("schönwald" -> [schonwald]), so plain searches are unaffected.
  test("an unpunctuated single term still targets both searchable fields"):
    val q = queryString("Schönwald")
    expect(
      q.contains("schönwald") && q.contains("FieldWithOptionalBoost(na") && q.contains(
        "FieldWithOptionalBoost(de"
      )
    )

  test("multiple whitespace-separated terms are combined with a bool filter"):
    val q = queryString("SF Schönwald")
    expect(
      q.startsWith("BoolQuery") && q.contains("MultiMatchQuery(sf,") && q.contains(
        "MultiMatchQuery(schönwald,"
      )
    )

  test("the count query uses the same conditions"):
    expect(Team("SF-Schönwald").countDef.query.get.toString.contains("Some(And)"))
