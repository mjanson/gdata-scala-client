package com.google.gdata.data

trait LinkNavigation {
  def links: List[Link]
  
  def link(rel: String): Option[Link] = {
    links.find(_.rel == rel)
  }
}
